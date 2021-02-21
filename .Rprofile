
## Default repo
local({
	r <- getOption("repos")
	r["CRAN"] <- "https://cloud.r-project.org/"
	options(repos=r)
})

options(stringsAsFactors=FALSE)
options(max.print=5000)
options(scipen=10)
options(editor="vim")
# options(show.signif.stars=FALSE)
options(menu.graphics=FALSE)
options(prompt="> ")
options(continue="... ")

q <- function (save="no", ...) {
	quit(save=save, ...)
}

utils::rc.settings(ipck=TRUE)

.First <- function(){
	if(interactive()){
		library(utils)
		timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))
	}
}

.Last <- function(){
	if(interactive()){
		hist_file <- Sys.getenv("R_HISTFILE")
		if(hist_file=="") hist_file <- "~/.RHistory"
		savehistory(hist_file)
	}
}

glr <- function() {
	lem <- geterrmessage()
	if(grepl("rlang::last_error", lem)) {
		lem <- rlang::last_error()$message
	}
	browseURL(paste0("https://stackoverflow.com/search?q=", URLencode(lem)))
}

#if(Sys.getenv("TERM") == "xterm-256color")
#library("colorout")

quiet_require <- function(a.package){
	suppressWarnings(suppressPackageStartupMessages(
		require(a.package, character.only=TRUE)
	))
}

auto.loads <- c("rmaria", "tidyverse", "data.table", "rutils", "colorout")

if(interactive()){
	if (!quiet_require("tidyverse")) {
		install.packages("tidyverse")
	}
	if (!quiet_require("data.table")) {
		install.packages("data.table")
	}
	if (!quiet_require("logging")) {
		install.packages("logging")
	}
	if (!quiet_require("magrittr")) {
		install.packages("magrittr")
	}
	if (!quiet_require("rmaria")) {
		devtools::install_github("Vongo/rmaria")
	}
	if (!quiet_require("rutils")) {
		devtools::install_github("Vongo/rutils")
	}
	if (!quiet_require("colorout")) {
		devtools::install_github("jalvesaq/colorout")
	}
}

print.data.frame <- function (x) {
	df <- as.data.frame(data.table::rbindlist(list(
		structure(c(" "=" ", lapply(x, function(c) paste(class(c), collapse=" / "))), class="data.frame", row.names="class"),
		cbind(seq(min(3, nrow(x))), head(x, min(3, nrow(x)))) %>% purrr::modify(as.character),
		if (nrow(x)>10) structure(as.list(c("", rep("-------", ncol(x)))), class="data.frame", row.names=c(NA, -1L)),
		c(sample(seq(4, nrow(x)-3), max(0, min(4, nrow(x)-6)))) %>% .[order(.)] %>% cbind(x[.,]) %>% purrr::modify(as.character),
		if (nrow(x)>10) structure(as.list(c("", rep("-------", ncol(x)))), class="data.frame", row.names=c(NA, -1L)),
		if (nrow(x)>3) cbind(seq(nrow(x)-(min(2, nrow(x)-4)), nrow(x)), tail(x, min(3, nrow(x)-3))) %>% purrr::modify(as.character)
	), use.names=FALSE))
	row_names <- `if`(all(is.na(suppressWarnings(as.numeric(rownames(x))))),
		c("", rownames(x)[suppressWarnings(as.numeric(unlist(df[-1, 1])))]),
		FALSE
	)
	if (any(is.na(row_names))) row_names[is.na(row_names)] <- ""
	base::print.data.frame(df, row.names=row_names)
}

ht <- function(d) rbind(head(d,10),tail(d,10))
hh <- function(d) d[1:5,1:5]

.dev <- function() {
	library(devtools)
	library(roxygen2)
}

.ccp <- function() {
	devtools::document()
	devtools::install()
}

.env <- new.env()
attach(.env)

.env$unrowname <- function(x) {
	rownames(x) <- NULL
	x
}

.env$unfactor <- function(df){
	id <- sapply(df, is.factor)
	df[id] <- lapply(df[id], as.character)
	df
}

if (interactive()) {
	rutils::ws(F)
	options(warn=1)
}

#theme_set(ggthemes::theme_few())
