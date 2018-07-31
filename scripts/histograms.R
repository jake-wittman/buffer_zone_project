mvmt_summary <- read.csv("data/summary_data_2017.csv", na=".")
mvmt_summary$total_path_distance <- as.numeric(as.character(mvmt_summary$total_path_distance))
mvmt_summary$displacement <- as.numeric(as.character(mvmt_summary$displacement))
mvmt_summary$mvmt_index <- as.numeric(as.character(mvmt_summary$mvmt_index))

hist(mvmt_summary$total_path_distance)
hist(mvmt_summary$displacement)
hist(mvmt_summary$mvmt_index)

