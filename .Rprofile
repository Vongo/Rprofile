
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

auto.loads <- c("rmaria", "tidyverse", "data.table", "rutils", "colorout", "glue", "gridExtra")

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
	if (nrow(x)==0) {
		base::print.data.frame(x)
	} else {
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
	invisible(x)
}

lsh <- function(split=TRUE, up=FALSE) {
	matches <- c("b", "Kb", "Mb", "Gb", "Tb", "Pb")
	ob <- ls(env=rlang::caller_env(n=1))
	bitsize <- sapply(ob, function(o) `if`(up, pryr::object_size, utils::object.size)(get(o, env=rlang::caller_env(n=1))))
	res <- data.table::data.table(
		name=ob,
		size=sapply(bitsize, function(size) {
			if (size>0) {
				coeff <- log10(size) %/% 3
				paste(round(size/(10^(coeff*3)),2), matches[coeff + 1])
			} else {
				"O b"
			}
		}),
		bitsize=bitsize
	)
	res <- res[order(-bitsize), ] |> data.table::as.data.table()
	rownames(res) <- NULL
	klass <- data.table::rbindlist(lapply(res$name, function(x) {data.table(name=x, class=class(eval(parse(text=x))))}), fill=TRUE)
	res <- data.table::merge.data.table(res, klass, by="name") |> as.data.table() # DT join wouldn't work
	if (split==TRUE) {
		classes <- res[, .(mb=max(bitsize)), class][order(-mb), class]
		for (cl in classes) {
			message(cl)
			base::print.data.frame(res[order(-bitsize)][class==cl, .(name, size)])
			cat("\n")
		}
	} else {
		base::print.data.frame(res[order(-bitsize), .(size=size[1], class=paste(class, collapse=", ")), name])
	}
	invisible(res)
}

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

# Theme black comes from https://gist.github.com/jslefche/eff85ef06b4705e6efbc
theme_black=function(base_size=12, base_family="") {
	suppressWarnings({theme_grey(base_size=base_size, base_family=base_family) %+replace%
		theme(
			# Specify axis options
			axis.line=element_blank(),
			axis.text.x=element_text(size=base_size*0.8, color="white", lineheight=0.9),
			axis.text.y=element_text(size=base_size*0.8, color="white", lineheight=0.9),
			axis.ticks=element_line(color="white", size=0.2),
			axis.title.x=element_text(size=base_size, color="white", margin=margin(0, 10, 0, 0)),
			axis.title.y=element_text(size=base_size, color="white", angle=90, margin=margin(0, 10, 0, 0)),
			axis.ticks.length=unit(0.3, "lines"),
			# Specify legend options
			legend.background=element_rect(color=NA, fill="black"),
			legend.key=element_rect(color="white", fill="black"),
			legend.key.size=unit(1.2, "lines"),
			legend.key.height=NULL,
			legend.key.width=NULL,
			legend.text=element_text(size=base_size*0.8, color="white"),
			legend.title=element_text(size=base_size*0.8, face="bold", hjust=0, color="white"),
			legend.position="right",
			legend.text.align=NULL,
			legend.title.align=NULL,
			legend.direction="vertical",
			legend.box=NULL,
			# Specify panel options
			panel.background=element_rect(fill="black", color=NA),
			panel.border=element_rect(fill=NA, color="white"),
			panel.grid.major=element_line(color="grey35"),
			panel.grid.minor=element_line(color="grey20"),
			panel.margin=unit(0.5, "lines"),
			# Specify facetting options
			strip.background=element_rect(fill="grey30", color="grey10"),
			strip.text.x=element_text(size=base_size*0.8, color="white"),
			strip.text.y=element_text(size=base_size*0.8, color="white",angle=-90),
			# Specify plot options
			plot.background=element_rect(color="black", fill="black"),
			plot.title=element_text(size=base_size*1.2, color="white"),
			plot.margin=unit(rep(1, 4), "lines")
		)
	})
}
theme_set(theme_black())
