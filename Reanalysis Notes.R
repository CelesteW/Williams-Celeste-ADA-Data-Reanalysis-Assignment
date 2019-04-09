library(readxl)
f <- "~/Desktop/TableA.xlsx"
A <- read_excel(f, sheet = 1, col_names = TRUE)
head(A)
f <- "~/Desktop/TableC.xlsx"
C <- read_excel(f, sheet = 1, col_names = TRUE)
head(C)
f <- "~/Desktop/TableD.xlsx"
D <- read_excel(f, sheet = 1, col_names = TRUE)
head(D)
f <- "~/Desktop/TableE.xlsx"
E <- read_excel(f, sheet = 1, col_names = TRUE)
head(E)
f <- "~/Desktop/TableF.xlsx"
F <- read_excel(f, sheet = 1, col_names = TRUE)
head(F)
f <- "~/Desktop/TableG.xlsx"
G <- read_excel(f, sheet = 1, col_names = TRUE)
head(G)

