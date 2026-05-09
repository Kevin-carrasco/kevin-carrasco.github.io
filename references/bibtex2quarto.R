bibtex_2academic <- function(bibfile,
                             outfold,
                             abstract = TRUE,
                             overwrite = FALSE) {
    
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(RefManageR, dplyr, stringr, anytime, tidyr, stringi, purrr)
    
    if (!dir.exists(outfold)) {
        dir.create(outfold, recursive = TRUE)
    }
    
    clean_bibtex_text <- function(x) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        x <- enc2utf8(x)
        
        x <- stringr::str_replace_all(x, "\\\\textquestiondown", "¿")
        x <- stringr::str_replace_all(x, "\\\\textexclamdown", "¡")
        
        x <- stringr::str_replace_all(x, "\\\\'\\{?a\\}?", "á")
        x <- stringr::str_replace_all(x, "\\\\'\\{?e\\}?", "é")
        x <- stringr::str_replace_all(x, "\\\\'\\{?i\\}?", "í")
        x <- stringr::str_replace_all(x, "\\\\'\\{?o\\}?", "ó")
        x <- stringr::str_replace_all(x, "\\\\'\\{?u\\}?", "ú")
        x <- stringr::str_replace_all(x, "\\\\~\\{?n\\}?", "ñ")
        
        x <- stringr::str_replace_all(x, "\\\\'\\{?A\\}?", "Á")
        x <- stringr::str_replace_all(x, "\\\\'\\{?E\\}?", "É")
        x <- stringr::str_replace_all(x, "\\\\'\\{?I\\}?", "Í")
        x <- stringr::str_replace_all(x, "\\\\'\\{?O\\}?", "Ó")
        x <- stringr::str_replace_all(x, "\\\\'\\{?U\\}?", "Ú")
        x <- stringr::str_replace_all(x, "\\\\~\\{?N\\}?", "Ñ")
        
        x <- stringr::str_replace_all(x, "\\\\&", "&")
        x <- stringr::str_replace_all(x, "\\\\_", "_")
        x <- stringr::str_replace_all(x, "\\\\%", "%")
        x <- stringr::str_replace_all(x, "\\\\#", "#")
        x <- stringr::str_replace_all(x, "\\\\-", "-")
        
        x <- stringr::str_replace_all(x, "[{}]", "")
        x <- stringr::str_replace_all(x, "\\\\", "")
        
        stringr::str_squish(x)
    }
    
    yaml_quote <- function(x) {
        x <- clean_bibtex_text(x)
        x <- gsub("\\\\", "\\\\\\\\", x)
        x <- gsub('"', '\\"', x, fixed = TRUE)
        paste0('"', x, '"')
    }
    
    slugify_filename <- function(x) {
        x %>%
            clean_bibtex_text() %>%
            stringi::stri_trans_general("Latin-ASCII") %>%
            stringr::str_to_lower() %>%
            stringr::str_replace_all("[^a-z0-9]+", "_") %>%
            stringr::str_replace_all("^_|_$", "") %>%
            stringr::str_sub(1, 60)
    }
    
    extract_image_urls <- function(annotation) {
        annotation <- as.character(annotation)
        
        if (is.na(annotation) || annotation == "") {
            return(character(0))
        }
        
        stringr::str_extract_all(
            annotation,
            stringr::regex(
                "https?://[^\\s\"'<>]+\\.(jpg|jpeg|png|gif|webp)(\\?[^\\s\"'<>]*)?",
                ignore_case = TRUE
            )
        )[[1]]
    }
    
    invert_author_names <- function(author_string) {
        if (is.null(author_string) || is.na(author_string) || author_string == "") {
            return("")
        }
        
        authors <- unlist(strsplit(author_string, "\\s+and\\s+", perl = TRUE))
        authors <- trimws(authors)
        
        surname_particles <- c(
            "da", "das", "de", "del", "della", "delle", "dels", "der",
            "di", "do", "dos", "du", "el", "la", "las", "le", "los",
            "van", "von", "y", "san", "santa", "st", "st."
        )
        
        authors <- vapply(authors, function(author_name) {
            author_name <- clean_bibtex_text(author_name)
            
            if (grepl(",", author_name, fixed = TRUE)) {
                comma_parts <- unlist(strsplit(author_name, ",", fixed = TRUE))
                surname <- trimws(comma_parts[1])
                given_names <- trimws(paste(comma_parts[-1], collapse = " "))
                
                if (given_names == "") return(surname)
                
                return(paste0(surname, ", ", given_names))
            }
            
            parts <- unlist(strsplit(author_name, "\\s+", perl = TRUE))
            
            if (length(parts) < 2) return(author_name)
            
            idx <- length(parts)
            surname_idx <- idx
            
            while (idx > 1) {
                prev_token <- tolower(parts[idx - 1])
                
                if (prev_token %in% surname_particles) {
                    surname_idx <- idx - 1
                    idx <- idx - 1
                } else {
                    break
                }
            }
            
            surname <- paste(parts[surname_idx:length(parts)], collapse = " ")
            given_names <- paste(parts[seq_len(surname_idx - 1)], collapse = " ")
            
            paste0(surname, ", ", given_names)
        }, character(1))
        
        paste(authors, collapse = " and ")
    }
    
    format_authors_display <- function(author_string) {
        author_inverted <- invert_author_names(author_string)
        auth_clean <- clean_bibtex_text(author_inverted)
        auth_vec <- trimws(unlist(strsplit(auth_clean, " and ", fixed = TRUE)))
        
        if (length(auth_vec) <= 1) {
            paste(auth_vec, collapse = "")
        } else if (length(auth_vec) == 2) {
            paste(auth_vec, collapse = " & ")
        } else {
            paste0(
                paste(auth_vec[-length(auth_vec)], collapse = "; "),
                " & ",
                auth_vec[length(auth_vec)]
            )
        }
    }
    
    fncols <- function(data, cname) {
        add <- cname[!cname %in% names(data)]
        
        if (length(add) != 0) {
            data[add] <- NA
        }
        
        data
    }
    
    mypubs <- RefManageR::ReadBib(
        bibfile,
        check = "warn",
        .Encoding = "UTF-8"
    ) %>%
        as.data.frame()
    
    mypubs <- fncols(
        mypubs,
        c(
            "author", "journal", "abstract", "annotation", "editor",
            "booktitle", "volume", "number", "pages", "address",
            "institution", "publisher", "doi", "isbn", "url", "year",
            "month", "school", "type", "keywords"
        )
    )
    
    mypubs <- mypubs %>%
        mutate(
            mainref = journal,
            mainref = replace_na(as.character(mainref), ""),
            abstract = replace_na(as.character(abstract), "(Abstract not available)"),
            month = replace_na(as.character(month), "jan"),
            annotation = replace_na(as.character(annotation), ""),
            bibtype = as.character(bibtype)
        )
    
    mypubs$annotation <- gsub("--- ", "---\n", mypubs$annotation)
    mypubs$annotation <- gsub(" ---", "\n---\n", mypubs$annotation)
    
    mypubs$title      <- clean_bibtex_text(mypubs$title)
    mypubs$abstract   <- clean_bibtex_text(mypubs$abstract)
    mypubs$mainref    <- clean_bibtex_text(mypubs$mainref)
    mypubs$annotation <- clean_bibtex_text(mypubs$annotation)
    
    url_fields <- c(
        "url_pdf", "url_preprint", "url_dataset",
        "url_project", "url_slides", "url_video", "url_poster"
    )
    
    for (field in url_fields) {
        mypubs[[field]] <- stringr::str_extract(
            mypubs$annotation,
            paste0(field, ':\\s*"([^"]+)"')
        )
        
        mypubs[[field]] <- stringr::str_replace_all(
            mypubs[[field]],
            paste0(field, ':\\s*"'),
            ""
        )
        
        mypubs[[field]] <- stringr::str_replace_all(
            mypubs[[field]],
            '"$',
            ""
        )
    }
    
    url_pattern <- paste0(
        "\\b(",
        paste(url_fields, collapse = "|"),
        "):\\s*\"[^\"]*\""
    )
    
    mypubs$annotation <- stringr::str_remove_all(mypubs$annotation, url_pattern)
    
    mypubs$editor <- gsub(" and ", ", ", mypubs$editor)
    mypubs$editor <- stringi::stri_replace_last_fixed(mypubs$editor, ",", " &")
    
    mypubs$keywords <- gsub(",", '","', mypubs$keywords)
    
    mypubs <- mypubs %>%
        mutate(
            categories = case_when(
                bibtype == "Article" ~ "Journal Article",
                bibtype == "Journal Article" ~ "Journal Article",
                bibtype == "Article in Press" ~ "Journal Article",
                
                bibtype == "InProceedings" & stringr::str_detect(outfold, "presentations") ~ "Presentation",
                bibtype == "InProceedings" ~ "Conference paper",
                bibtype == "Proceedings" ~ "Conference paper",
                bibtype == "Conference" ~ "Conference paper",
                bibtype == "Conference Paper" ~ "Conference paper",
                
                bibtype == "Thesis" ~ "Manuscript",
                bibtype == "MastersThesis" ~ "Manuscript",
                bibtype == "PhdThesis" ~ "Manuscript",
                bibtype == "Manual" ~ "Report",
                bibtype == "TechReport" ~ "Report",
                bibtype == "Book" ~ "Book",
                bibtype == "InCollection" ~ "Book Section",
                bibtype == "InBook" ~ "Book Section",
                
                bibtype == "Presentation" ~ "Presentation",
                bibtype == "Misc" & stringr::str_detect(outfold, "presentations") ~ "Presentation",
                bibtype == "Misc" ~ "Report",
                
                bibtype == "Preprint" ~ "Manuscript",
                TRUE ~ "0"
            )
        )
    
    create_md <- function(x) {
        
        year <- ifelse(
            !is.na(x[["year"]]) && x[["year"]] != "",
            x[["year"]],
            "2999"
        )
        
        month <- ifelse(
            !is.na(x[["month"]]) && x[["month"]] != "",
            x[["month"]],
            "jan"
        )
        
        date2 <- paste0(year, "-", month, "-01")
        
        filename <- paste0(
            year,
            "_",
            slugify_filename(x[["title"]]),
            ".qmd"
        )
        
        fileConn <- file.path(outfold, filename)
        
        if (file.exists(fileConn) && !overwrite) {
            return(invisible(NULL))
        }
        
        image_urls <- extract_image_urls(x[["annotation"]])
        
        if (length(image_urls) > 0) {
            for (img in image_urls) {
                x[["annotation"]] <- stringr::str_replace_all(
                    x[["annotation"]],
                    stringr::fixed(img),
                    ""
                )
            }
            
            x[["annotation"]] <- stringr::str_squish(x[["annotation"]])
        }
        
        publication <- x[["mainref"]]
        
        if (!is.na(x[["editor"]]) && x[["editor"]] != "") {
            publication <- paste0(publication, " In ", x[["editor"]], ": ")
        }
        
        if (!is.na(x[["booktitle"]]) && x[["booktitle"]] != "") {
            publication <- paste0(publication, x[["booktitle"]])
        }
        
        if (!is.na(x[["volume"]]) && x[["volume"]] != "") {
            publication <- paste0(publication, ", ", x[["volume"]])
        }
        
        if (!is.na(x[["type"]]) && x[["type"]] != "") {
            publication <- paste0(publication, x[["type"]], " ")
        }
        
        if (!is.na(x[["number"]]) && x[["number"]] != "") {
            publication <- paste0(publication, "(", x[["number"]], ")")
        }
        
        if (!is.na(x[["pages"]]) && x[["pages"]] != "") {
            publication <- paste0(publication, " ", x[["pages"]], " ")
        }
        
        if (!is.na(x[["school"]]) && x[["school"]] != "") {
            publication <- paste0(publication, "- ", x[["school"]])
        }
        
        if (!is.na(x[["address"]]) && x[["address"]] != "") {
            publication <- paste0(publication, ". ", x[["address"]])
        }
        
        if (!is.na(x[["institution"]]) && x[["institution"]] != "") {
            publication <- paste0(publication, " ", x[["institution"]])
        }
        
        if (!is.na(x[["publisher"]]) && x[["publisher"]] != "") {
            publication <- paste0(publication, ": ", x[["publisher"]])
        }
        
        if (!is.na(x[["doi"]]) && x[["doi"]] != "") {
            publication <- paste0(
                publication,
                " [https://doi.org/",
                x[["doi"]],
                "](https://doi.org/",
                x[["doi"]],
                ")"
            )
        }
        
        if (!is.na(x[["isbn"]]) && x[["isbn"]] != "") {
            publication <- paste0(publication, ". ISBN: ", x[["isbn"]])
        }
        
        publication <- clean_bibtex_text(publication)
        
        author_display <- format_authors_display(x[["author"]])
        
        write("---", fileConn)
        write(paste0("title: ", yaml_quote(x[["title"]])), fileConn, append = TRUE)
        write(paste0("date: ", yaml_quote(anytime::anydate(date2))), fileConn, append = TRUE)
        write(paste0("author: ", yaml_quote(author_display)), fileConn, append = TRUE)
        write(paste0("categories: [", yaml_quote(x[["categories"]]), "]"), fileConn, append = TRUE)
        write(paste0("publication: ", yaml_quote(publication)), fileConn, append = TRUE)
        write(paste0("publication_short: ", yaml_quote(publication)), fileConn, append = TRUE)
        write(paste0("abstract: ", yaml_quote(x[["abstract"]])), fileConn, append = TRUE)
        write("abstract_short: \"\"", fileConn, append = TRUE)
        
        annotation_links <- list()
        
        x[["annotation"]] <- gsub(
            "- icon: graduation-cap.*?(?=- icon|$)",
            "",
            x[["annotation"]],
            perl = TRUE
        )
        
        matches <- stringr::str_match_all(
            x[["annotation"]],
            "- icon: ([^\\n]+)\\s*\\n\\s*(icon_pack: [^\\n]+\\s*\\n)?\\s*(name: [^\\n]+\\s*\\n)?\\s*(web:|href:) ([^\\n]+)"
        )[[1]]
        
        if (nrow(matches) > 0) {
            annotation_links <- apply(matches, 1, function(row) {
                icon_name <- row[2]
                url <- row[6]
                
                if (grepl("github\\.com", url)) {
                    icon_name <- "github"
                } else if (!is.na(icon_name) && icon_name == "file") {
                    icon_name <- "file-pdf-fill"
                }
                
                list(icon = icon_name, href = url)
            })
        }
        
        x[["annotation"]] <- gsub(
            "(\\n|^)#?\\s*-\\s*icon:\\s*[^\\n]+\\s*\\n\\s*(icon_pack:\\s*[^\\n]+\\s*\\n)?\\s*(name:\\s*[^\\n]+\\s*\\n)?\\s*(web:|href:)\\s*[^\\n]+\\n?",
            "",
            x[["annotation"]],
            perl = TRUE
        )
        
        x[["annotation"]] <- gsub(
            "\\n?links:\\s*(\\n\\s*-.*)?",
            "",
            x[["annotation"]],
            perl = TRUE
        )
        
        x[["annotation"]] <- gsub(
            "url_[^:]+:\\s*\"[^\"]*\"\\s*",
            "",
            x[["annotation"]]
        )
        
        x[["annotation"]] <- gsub("\n{2,}", "\n\n", x[["annotation"]])
        x[["annotation"]] <- trimws(x[["annotation"]])
        
        icon_map <- list(
            url_slides   = list(icon = "file-slides-fill"),
            url_video    = list(icon = "camera-video-fill"),
            url_poster   = list(icon = "image-fill"),
            url_pdf      = list(icon = "file-pdf-fill"),
            url_preprint = list(icon = "files-alt"),
            url_dataset  = list(icon = "database"),
            url_project  = list(icon = "archive")
        )
        
        links <- list()
        
        for (field in names(icon_map)) {
            if (!is.null(x[[field]]) && !is.na(x[[field]]) && x[[field]] != "") {
                links <- append(
                    links,
                    list(
                        list(
                            icon = icon_map[[field]]$icon,
                            href = x[[field]]
                        )
                    )
                )
            }
        }
        
        all_links <- c(annotation_links, links)
        
        all_links <- purrr::keep(all_links, function(z) {
            !is.null(z$icon) &&
                !is.na(z$icon) &&
                z$icon != "" &&
                !is.null(z$href) &&
                !is.na(z$href) &&
                z$href != ""
        })
        
        write("about:", fileConn, append = TRUE)
        write("  template: marquee", fileConn, append = TRUE)
        
        if (length(all_links) > 0) {
            write("  links:", fileConn, append = TRUE)
            
            for (link in all_links) {
                write(paste0("    - icon: ", link[["icon"]]), fileConn, append = TRUE)
                write(paste0("      href: ", link[["href"]]), fileConn, append = TRUE)
            }
        }
        
        write("---", fileConn, append = TRUE)
        
        if (length(image_urls) > 0) {
            write("", fileConn, append = TRUE)
            write(paste0("![](", image_urls[1], ")"), fileConn, append = TRUE)
            write("", fileConn, append = TRUE)
        }
        
        if (!is.na(x[["annotation"]]) && x[["annotation"]] != "") {
            write(x[["annotation"]], fileConn, append = TRUE)
        }
        
        citation_text <- paste0(
            author_display,
            " (",
            ifelse(!is.na(x[["year"]]) && x[["year"]] != "", x[["year"]], "s.f."),
            "). *",
            x[["title"]],
            "*"
        )
        
        if (publication != "") {
            citation_text <- paste0(citation_text, ". ", publication)
        }
        
        write("\n\n::: {.callout-note title=\"How to cite this work\"}\n", fileConn, append = TRUE)
        write(citation_text, fileConn, append = TRUE)
        write("\n:::\n", fileConn, append = TRUE)
        
        invisible(fileConn)
    }
    
    invisible(lapply(seq_len(nrow(mypubs)), function(i) {
        try(create_md(mypubs[i, ]), silent = TRUE)
    }))
    
    invisible(mypubs)
}



# Running the function for publications

my_bibfile <- "references/publications/publicaciones.bib"
out_fold   <- "references/publications/posts"
bibtex_2academic(bibfile  = my_bibfile,
                  outfold   = out_fold,
                  abstract  = TRUE,
                  overwrite = TRUE)


# Running the function for presentations

#my_bibfile <- "references/presentations/presentations.bib"
#out_fold   <- "references/presentations/posts"
#bibtex_2academic(bibfile  = my_bibfile,
#                  outfold   = out_fold,
#                  abstract  = TRUE,
#                  overwrite = TRUE)





# Run this in R (within your website project folder): 

# source("references/bibtex2quarto.R")

