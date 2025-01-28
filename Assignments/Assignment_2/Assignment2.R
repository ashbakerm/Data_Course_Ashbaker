# 4 - List all of the .csv files found in the Data/ directory and store the list in an object called “csv_files”
csv_files <- list.files(path = "Data", pattern = "\\.csv$", full.names = TRUE)
csv_files

# 5 - Find how many files match that description using the length() function
num_csv_files <- length(csv_files)
cat("Number of .csv files:", num_csv_files, "\n")

# 6 - Open the wingspan_vs_mass.csv file and store the contents as an R object named “df” using the read.csv() function
df <- read.csv("Data/wingspan_vs_mass.csv")
df

# 7 - Inspect the first 5 lines of this data set using the head() function
cat("First 5 rows of wingspan_vs_mass.csv:\n")
head(df, 5)

# 8 - Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)
b_files <- list.files(path = "Data", pattern = "^b", full.names = TRUE, recursive = TRUE)
cat("Files that begin with 'b':\n", b_files, "\n")
b_files

# 9 - Display the first line of each file beginning with 'b'
cat("First line of each file beginning with 'b':\n")
for (file in b_files) {
  first_line <- readLines(file, n = 1) # Read the first line
  cat("File:", file, "\n")
  cat("First Line:", first_line, "\n\n")
}

# 10 - Display the first line of all files that end in “.csv”
cat("First line of each .csv file:\n")
for (file in csv_files) {
  first_line <- readLines(file, n = 1) # Read the first line
  cat("File:", file, "\n")
  cat("First Line:", first_line, "\n\n")
}