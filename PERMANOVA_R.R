# Calculate Bray-Curtis distance
library(vegan)    # For calculating Bray-Curtis distance
library(readr)    # For data reading (optional)

# Read expression matrix (rows as Unigenes, columns as samples)
# Replace "your_file.xls" with actual filename
expr <- read.delim("your_file.xls", header = TRUE, row.names = 1, check.names = FALSE)

# Transpose expression matrix (samples as rows, Unigenes as columns)
expr_t <- t(expr)

# Calculate Bray-Curtis distance matrix (sample vs sample)
bray_curtis_dist <- vegdist(expr_t, method = "bray")

# Convert to standard matrix format
bray_curtis_matrix <- as.matrix(bray_curtis_dist)

# Write sample-wise Bray-Curtis distance matrix
write.table(bray_curtis_matrix, file = "BCD.xls", sep = "\t", quote = FALSE, col.names = NA)

##### PERMANOVA (for different groups like BC, BS, FS, etc.)

# Load required packages
library(vegan)

# Read distance matrix
# Replace "your_distance_file.xls" with actual filename
dist_matrix <- read.table("your_distance_file.xls", header = TRUE, row.names = 1, check.names = FALSE)
dist_matrix <- as.dist(as.matrix(dist_matrix))

# Create grouping information
group_info <- data.frame(
  sample = rownames(as.matrix(dist_matrix)),
  group = substr(rownames(as.matrix(dist_matrix)), 1, 2)
)

# ========== 1. Overall PERMANOVA analysis ==========
adonis_result <- adonis2(dist_matrix ~ group, data = group_info, permutations = 999, method = "bray")
print(adonis_result)

# Extract overall results table (first two rows for model and residual)
overall_df <- as.data.frame(adonis_result)
overall_df <- cbind(term = rownames(overall_df), overall_df)
rownames(overall_df) <- NULL  # Clear row names

# ========== 2. Pairwise PERMANOVA function ==========
pairwise_adonis <- function(dist_matrix, group, permutations = 999, p.adjust.method = "BH") {
  combinations <- combn(unique(group), 2)
  result_list <- apply(combinations, 2, function(x) {
    grp1 <- x[1]
    grp2 <- x[2]
    idx <- group %in% c(grp1, grp2)
    sub_dist <- as.dist(as.matrix(dist_matrix)[idx, idx])
    sub_group <- factor(group[idx])
    ad <- adonis2(sub_dist ~ sub_group, permutations = permutations)
    data.frame(
      group1 = grp1,
      group2 = grp2,
      F.Model = ad$F[1],
      R2 = ad$R2[1],
      p.value = ad$`Pr(>F)`[1]
    )
  })
  result_df <- do.call(rbind, result_list)
  result_df$p.adj <- p.adjust(result_df$p.value, method = p.adjust.method)
  return(result_df)
}

# ========== 3. Perform pairwise and save all results ==========
pairwise_result <- pairwise_adonis(dist_matrix, group_info$group)

# Write combined output to TXT (including both overall and pairwise results)
sink("permanova_full_results.txt")

cat(">>> Overall PERMANOVA result:\n\n")
print(adonis_result)

cat("\n\n>>> Pairwise PERMANOVA result:\n\n")
print(pairwise_result)

sink()

# ========== 4. For bamboo shoot four groups analysis ========== 
# Load required packages
library(vegan)

# ========== 1. Read distance matrix ========== 
# Replace "your_distance_file.xls" with actual filename
dist_matrix <- read.table("your_distance_file.xls", header = TRUE, row.names = 1, check.names = FALSE)
dist_matrix <- as.dist(as.matrix(dist_matrix))  # Convert to dist object

# ========== 2. Read grouping information ========== 
# Replace "group_file.tsv" with actual filename
group_info <- read.table("group_file.tsv", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
colnames(group_info) <- c("sample", "group")  # Add column names

# Ensure order matches distance matrix (critical)
group_info <- group_info[match(labels(dist_matrix), group_info$sample), ]

# ========== 3. Overall PERMANOVA analysis ========== 
adonis_result <- adonis2(dist_matrix ~ group, data = group_info, permutations = 999, method = "bray")
print(adonis_result)

# Extract results table
overall_df <- as.data.frame(adonis_result)
overall_df <- cbind(term = rownames(overall_df), overall_df)
rownames(overall_df) <- NULL

# ========== 4. Pairwise PERMANOVA function definition ========== 
# (Same as above, included for completeness)

# ========== 5. Perform Pairwise PERMANOVA ========== 
pairwise_result <- pairwise_adonis(dist_matrix, group_info$group)

# ========== 6. Write results to TXT file ========== 
sink("permanova_full_results.txt")

cat(">>> Overall PERMANOVA result:\n\n")
print(adonis_result)

cat("\n\n>>> Pairwise PERMANOVA result:\n\n")
print(pairwise_result)

sink()