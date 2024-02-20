convert_win_path <- function(path = readClipboard()) {
    path %>% 
        gsub(pattern = "\"", replacement = "") %>% 
        strsplit(split = "\\\\") %>% 
        sapply(
            function(x) {do.call(file.path, as.list(x))}
        )
}
