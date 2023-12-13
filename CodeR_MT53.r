install.packages("naniar")
install.packages("dplyr")
library(naniar)
library(dplyr)

#3. Tiền xử lý số liệu
Intel_CPUs <- read.csv("~/Documents/HOCKY231/XSTK/BTL XSTK/Intel_CPUs.csv", stringsAsFactors=TRUE, header=TRUE)
head(Intel_CPUs, 3)

#tao 1 dataframe chỉ có 9 cột có giá trị thống kê để tương tác
new_Intel_CPUs<-Intel_CPUs[,c ("Lithography", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "TDP", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Instruction_Set", "Vertical_Segment")]

#Hien thi bang so lieu
View(Intel_CPUs)
View(new_Intel_CPUs)

# dữ liệu khuyết chuyển thành NA
new_Intel_CPUs[new_Intel_CPUs==""] = NA
new_Intel_CPUs[new_Intel_CPUs=="N/A"] = NA

#Thống kê dữ liệu khuyết
miss_var_summary(new_Intel_CPUs)
apply(is.na(new_Intel_CPUs),2,sum)

#XỬ LÝ DỮ LIỆU CỘT Lithography
#Xóa dữ liệu NA
new_Intel_CPUs <- subset(new_Intel_CPUs, !is.na(new_Intel_CPUs$Lithography))

#kiểm tra dữ liệu cột Lithography
class(new_Intel_CPUs$Lithography)

#kiểm tra dữ liệu có đơn vị khác
# Lấy các giá trị duy nhất trong cột Lithography
unique_values <- unique(new_Intel_CPUs$Lithography)

# Tạo một vector lưu trữ các đơn vị khác nhau
different_units <- character(0)

# Lặp qua các giá trị trong biến unique và kiểm tra đơn vị
for (value in unique_values) {
  if (!grepl(" nm$", value)) {
    different_units <- c(different_units, value)
  }
}

if (length(different_units) > 0) {
  cat("Có các đơn vị khác nhau trong cột Lithography:\n")
  cat(paste(different_units, collapse = ", "), "\n")
} else {
  cat("Tất cả giá trị trong cột Lithography có đơn vị là 'nm'.\n")
}
rm(unique_values, different_units, value)

#xu ly dinh dang du lieu
new_Intel_CPUs$Lithography <- sub(" nm", "", new_Intel_CPUs$Lithography)
new_Intel_CPUs$Lithography <- as.numeric(new_Intel_CPUs$Lithography)

#XỬ LÝ DỮ LIỆU CỘT nb_of_Threads
#Thay thế dữ liệu trống thành giá trị trung bình
class(new_Intel_CPUs$nb_of_Threads)
median_value <- median(new_Intel_CPUs$nb_of_Threads, na.rm = TRUE)
new_Intel_CPUs$nb_of_Threads[is.na(new_Intel_CPUs$nb_of_Threads)] <- median_value
rm(median_value)

#XỬ LÝ DỮ LIỆU CỘT Processor_Base_Frequency
#Xóa dữ liệu NA
new_Intel_CPUs <- subset(new_Intel_CPUs, !is.na(new_Intel_CPUs$Processor_Base_Frequency))

#kiểm tra dữ liệu cột Processor_Base_Frequency
class(new_Intel_CPUs$Processor_Base_Frequency)

#kiểm tra dữ liệu có đơn vị khác
# Lấy các giá trị duy nhất trong cột Processor_Base_Frequency
unique_values <- unique(new_Intel_CPUs$Processor_Base_Frequency)

# Tạo một vector lưu trữ các đơn vị khác nhau
different_units <- character(0)

# Lặp qua các giá trị trong biến unique và kiểm tra đơn vị
for (value in unique_values) {
  if (!grepl(" GHz$", value)) {
    different_units <- c(different_units, value)
  }
}

if (length(different_units) > 0) {
  cat("Có các đơn vị khác nhau trong cột Processor_Base_Frequency:\n")
  cat(paste(different_units, collapse = ", "), "\n")
} else {
  cat("Tất cả giá trị trong cột Processor_Base_Frequency có đơn vị là 'GHz'.\n")
}
rm(unique_values, different_units, value)

#hàm chuyển đổi MHz qua GHz
convertMHztoGHz <- function(data) {
  # Loại bỏ " GHz" và " MHz" và chuyển đổi thành numeric
  numeric_values <- as.numeric(gsub(" GHz| MHz", "", data))
  
  # Kiểm tra xem giá trị có " GHz" không
  is_GHz <- grepl(" GHz", data)
  
  # Chia cho 1000 cho các giá trị "MHz" và giữ nguyên các giá trị "GHz"
  GHz_values <- ifelse(is_GHz, numeric_values, numeric_values / 1000)
  
  # Kết quả là một vector chứa giá trị GHz
  return(GHz_values)
}


# Sử dụng hàm để chuyển đổi
new_Intel_CPUs$Processor_Base_Frequency <- convertMHztoGHz(new_Intel_CPUs$Processor_Base_Frequency)


#XỬ LÝ DỮ LIỆU CỘT TDP
#kiểm tra dữ liệu cột TDP
class(new_Intel_CPUs$TDP)

#kiểm tra dữ liệu có đơn vị khác
# Lấy các giá trị duy nhất trong cột TDP  
unique_values <- unique(new_Intel_CPUs$TDP)

# Tạo một vector lưu trữ các đơn vị khác nhau
different_units <- character(0)

# Lặp qua các giá trị duy nhất và kiểm tra đơn vị
for (value in unique_values) {
  if (!grepl(" W$", value)) {
    different_units <- c(different_units, value)
  }
}

if (length(different_units) > 0) {
  cat("Có các đơn vị khác nhau trong cột TDP:\n")
  cat(paste(different_units, collapse = ", "), "\n")
} else {
  cat("Tất cả giá trị trong cột TDP có đơn vị là 'W'.\n")
}

# Loại bỏ dấu "W" từ cột "TDP"
new_Intel_CPUs$TDP <- gsub(" W", "", new_Intel_CPUs$TDP)

#chuyen doi du lieu thanh numeric
new_Intel_CPUs$TDP <- as.numeric(as.character(new_Intel_CPUs$TDP))

# Tính giá trị trung vị và thay thế giá trị NA
median_value <- median(new_Intel_CPUs$TDP, na.rm = TRUE)
new_Intel_CPUs$TDP[is.na(new_Intel_CPUs$TDP)] <- median_value
rm(different_units, median_value, unique_values, value)

#XỬ LÝ DỮ LIỆU CỘT Max_Memory_Size
#kiểm tra dữ liệu cột Max_Memory_Size
class(new_Intel_CPUs$Max_Memory_Size)

#kiểm tra dữ liệu có đơn vị khác
# Lấy các giá trị duy nhất trong cột Max_Memory_Size
unique_values <- unique(new_Intel_CPUs$Max_Memory_Size)

# Tạo một vector lưu trữ các đơn vị khác nhau
different_units <- character(0)

# Lặp qua các giá trị duy nhất và kiểm tra đơn vị
for (value in unique_values) {
  if (!grepl(" GB$", value)) {
    different_units <- c(different_units, value)
  }
}

if (length(different_units) > 0) {
  cat("Có các đơn vị khác nhau trong cột Max_Memory_Size:\n")
  cat(paste(different_units, collapse = ", "), "\n")
} else {
  cat("Tất cả giá trị trong cột Max_Memory_Size có đơn vị là 'GB'.\n")
}
rm(different_units, unique_values, value)

#hàm chuyển đổi TB qua GB
convertTBtoGB <- function(data) {
  # Loại bỏ " GHz" và " MHz" và chuyển đổi thành numeric
  numeric_values <- as.numeric(gsub(" GB| TB", "", data))
  
  # Kiểm tra xem giá trị có " GHz" không
  is_GB <- grepl(" GB", data)
  
  # Nhan cho 1000 cho các giá trị "GB" và giữ nguyên các giá trị "GB"
  GB_values <- ifelse(is_GB, numeric_values, numeric_values * 1000)
  
  # Kết quả là một vector chứa giá trị GHz
  return(GB_values)
}

# Sử dụng hàm để chuyển đổi
new_Intel_CPUs$Max_Memory_Size <- convertTBtoGB(new_Intel_CPUs$Max_Memory_Size)

#Thay thế dữ liệu trống thành giá trị trung bình
class(new_Intel_CPUs$Max_Memory_Size)
median_value <- median(new_Intel_CPUs$Max_Memory_Size, na.rm = TRUE)
new_Intel_CPUs$Max_Memory_Size[is.na(new_Intel_CPUs$Max_Memory_Size)] <- median_value
rm(median_value)


#XỬ LÝ DỮ LIỆU CỘT Max_nb_of_Memory_Channels
#Thay thế dữ liệu trống thành giá trị trung bình
class(new_Intel_CPUs$Max_nb_of_Memory_Channels)
median_value <- median(new_Intel_CPUs$Max_nb_of_Memory_Channels, na.rm = TRUE)
new_Intel_CPUs$Max_nb_of_Memory_Channels[is.na(new_Intel_CPUs$Max_nb_of_Memory_Channels)] <- median_value
rm(median_value)


#XỬ LÝ DỮ LIỆU CỘT Instruction_Set
#Xóa dữ liệu NA & Itanium
new_Intel_CPUs <- subset(new_Intel_CPUs, !is.na(new_Intel_CPUs$Instruction_Set))
new_Intel_CPUs <- subset(new_Intel_CPUs, new_Intel_CPUs$Instruction_Set != "Itanium 64-bit")

#kiểm tra dữ liệu cột Instruction_Set
class(new_Intel_CPUs$Instruction_Set)

#kiểm tra dữ liệu có đơn vị khác
unique(new_Intel_CPUs$Instruction_Set)

#loại bỏ level không có trong dữ liệu
new_Intel_CPUs$Instruction_Set <- droplevels(new_Intel_CPUs$Instruction_Set)

#chuyen du lieu thanh numeric
new_Intel_CPUs$Instruction_Set <- as.numeric(gsub("-bit", "", new_Intel_CPUs$Instruction_Set))

#THỐNG KÊ MÔ TẢ
#làm rõ dữ liệu(data visualizaiton) (chuyen thanh dang logarit tự nhiên)
new_Intel_CPUs[,c ("Lithography", "nb_of_Cores", "nb_of_Threads","TDP", "Max_Memory_Size")] = log(new_Intel_CPUs[,c ("Lithography", "nb_of_Cores", "nb_of_Threads", "TDP", "Max_Memory_Size")])
colnames(new_Intel_CPUs) <- c("log_Lithography", "log_nb_of_Cores", "log_nb_of_Threads", "Processor_Base_Frequency", "log_TDP", "log_Max_Memory_Size", "Max_nb_of_Memory_Channels", "Instruction_Set", "Vertical_Segment")
head(new_Intel_CPUs, 3)

#Tính các giá trị thống kê mô tả
mean <- apply(new_Intel_CPUs[,c("log_Lithography", "log_nb_of_Cores", "log_nb_of_Threads", "Processor_Base_Frequency", "log_TDP", "log_Max_Memory_Size", "Max_nb_of_Memory_Channels", "Instruction_Set")], 2, mean)
median <- apply (new_Intel_CPUs[,c("log_Lithography", "log_nb_of_Cores", "log_nb_of_Threads", "Processor_Base_Frequency", "log_TDP", "log_Max_Memory_Size", "Max_nb_of_Memory_Channels", "Instruction_Set")], 2, median)
sd <- apply(new_Intel_CPUs[,c("log_Lithography", "log_nb_of_Cores", "log_nb_of_Threads", "Processor_Base_Frequency", "log_TDP", "log_Max_Memory_Size", "Max_nb_of_Memory_Channels", "Instruction_Set")], 2, sd)
min <- apply(new_Intel_CPUs[,c("log_Lithography", "log_nb_of_Cores", "log_nb_of_Threads", "Processor_Base_Frequency", "log_TDP", "log_Max_Memory_Size", "Max_nb_of_Memory_Channels", "Instruction_Set")], 2, min)
max <- apply(new_Intel_CPUs[,c("log_Lithography", "log_nb_of_Cores", "log_nb_of_Threads", "Processor_Base_Frequency", "log_TDP", "log_Max_Memory_Size", "Max_nb_of_Memory_Channels", "Instruction_Set")], 2, max)
Intel_CPUs_data <- cbind(data.frame(mean, median, sd, min, max))
View(Intel_CPUs_data)

#bảng thống kê số lượng cho từng chủng loại biến phân loại
table(new_Intel_CPUs$Max_nb_of_Memory_Channels)
table(new_Intel_CPUs$Instruction_Set)
table(new_Intel_CPUs$Vertical_Segment)

#Vẽ biểu đồ histogram
hist(new_Intel_CPUs$log_TDP, xlab = "log_TDP", main="1) Histogram of log_TDP", ylim =c(0, 500), labels = TRUE)

#Bài toán kiểm định 1 mẫu
sample <- new_Intel_CPUs$log_TDP
alpha <- 1 - 0.95
t.test(sample , conf.level = 1 - alpha)
rm(sample, alpha)

#Bài toán kiểm định 2 mẫu
table(new_Intel_CPUs$Vertical_Segment)

# Trích xuất hai mẫu con từ log_TDP theo Vertical_Segment là Mobile và Server
sample1 <- new_Intel_CPUs$log_TDP[new_Intel_CPUs$Vertical_Segment == "Mobile"]
sample2 <- new_Intel_CPUs$log_TDP[new_Intel_CPUs$Vertical_Segment == "Server"]
# Mức ý nghĩa của kiểm định, cho trước = 5%
alpha <- 0.05
# Dùng hàm t.test() để kiểm định trung bình cho hai mẫu
t.test(sample1 , sample2 , var.equal = (var(sample1) == var(sample2)),conf.level = 1 - alpha, alternative = "two.sided")
rm(sample1, sample2, alpha)

#vẽ các phân phối của biến Processor_Base_Frequency lần lượt theo các biến phan loai
boxplot(log_TDP ~ Max_nb_of_Memory_Channels, main = "2) Boxplot of log_TDP for each category of Max_nb_of_Memory_Channels", data = new_Intel_CPUs,col=c(2,3,4,5,6,7,8))
boxplot(log_TDP ~ Instruction_Set, main = "3) Boxplot of log_TDP for each category of Instruction_Set", data = new_Intel_CPUs,col=c(2,3))
boxplot(log_TDP ~ Vertical_Segment, main = "4) Boxplot of log_TDP for each category of Vertical_Segment", data = new_Intel_CPUs,col=c(2,3,4,5,6,7,8))


#vẽ các phân phối của biến Processor_Base_Frequency lần lượt theo các biến
pairs(new_Intel_CPUs[c("log_TDP", "log_Lithography")], pch = 16, col = "red", main = "5) pairs of log_TDP and log_Lithography")
pairs(new_Intel_CPUs[c("log_TDP", "log_nb_of_Cores")], pch = 16, col = "blue", main = "7) pairs of log_TDP and log_nb_of_Cores")
pairs(new_Intel_CPUs[c("log_TDP", "log_nb_of_Threads")], pch = 16, col = "purple", main = "8) pairs of log_TDP and log_nb_of_Threads")
pairs(new_Intel_CPUs[c("log_TDP", "Processor_Base_Frequency")], pch = 16, col = "purple", main = "8) pairs of log_TDP and Processor_Base_Frequency")
pairs(new_Intel_CPUs[c("log_TDP", "log_Max_Memory_Size")], pch = 16, col = "pink", main = "10) pairs of log_TDP and log_Max_Memory_Size")

#XÂY DỰNG MÔ HÌNH HỒI QUY TUYẾN TÍNH
#chuyen du lieu thanh factor
new_Intel_CPUs$Instruction_Set = as.factor(new_Intel_CPUs$Instruction_Set)
new_Intel_CPUs$Max_nb_of_Memory_Channels = as.factor(new_Intel_CPUs$Max_nb_of_Memory_Channels)
new_Intel_CPUs$Vertical_Segment = as.factor(new_Intel_CPUs$Vertical_Segment)

m1 = lm(log_TDP ~ log_Lithography  + log_nb_of_Cores + log_nb_of_Threads + Processor_Base_Frequency + log_Max_Memory_Size + Max_nb_of_Memory_Channels + Instruction_Set + Vertical_Segment, data = new_Intel_CPUs)
summary(m1)
#do Vertical_SegmentServer co Pr(>|t|)  > muc y nghia 0.05 nen can nhac loai bo ra khoi mo hinh 2
m2 = lm(log_TDP ~ log_Lithography  + log_nb_of_Cores + log_nb_of_Threads + Processor_Base_Frequency + log_Max_Memory_Size + Max_nb_of_Memory_Channels + Instruction_Set , data = new_Intel_CPUs)
summary(m2)

anova(m1,m2)

#đồ thị biểu thị sai số hồi quy (residuals) và giá trị dự báo (fitted values)
par(mfrow = c(2,2))
plot(m1)
#Đồ thị thứ 1 (Residuals vs Fitted) cho thấy giả thiết về tính tuyến tính của dữ liệu hơi bị vi phạm. Tuy nhiên giả thiết trung bình của phần dư có thể coi là thỏa mãn
#Đồ thị Normal Q-Q cho thấy giả thiết phần dư có phân phối chuẩn được thỏa mãn.
#Đồ thị (Scale - Location) cho ta thấy rằng giả thiết về tính đồng nhất của phương sai tương đối thỏa mãn.
#Đồ thị thứ tư chỉ ra có các quan trắc thứ 14, 30, 31 có thể là các điểm có ảnh hưởng cao trong bộ dữ liệu.

#tính giá trị thống kê miêu tả
by(new_Intel_CPUs$log_TDP, new_Intel_CPUs$Vertical_Segment, summary)

#ve bieu do 
par(mfrow = c(1,1))
boxplot(log_TDP ~ Vertical_Segment, main = "11) Boxplot of log_TDP for each category of Vertical_Segment", data = new_Intel_CPUs,col=c(2,3,4,5,6,7,8))

#kiểm tra giả định về phân phối chuẩn
Desktop<-subset(new_Intel_CPUs,new_Intel_CPUs$Vertical_Segment =="Desktop")
qqnorm(Desktop$log_TDP)
qqline(Desktop$log_TDP)
shapiro.test(Desktop$log_TDP) #p-value < muc y nghia 0,05 nen ta bac bo gia thiet h0

Mobile<-subset(new_Intel_CPUs,new_Intel_CPUs$Vertical_Segment =="Mobile")
qqnorm(Mobile$log_TDP)
qqline(Mobile$log_TDP)
shapiro.test(Mobile$log_TDP)

Sever<-subset(new_Intel_CPUs,new_Intel_CPUs$Vertical_Segment == "Server")
qqnorm(Sever$log_TDP)
qqline(Sever$log_TDP)
shapiro.test(Sever$log_TDP)

Embedded<-subset(new_Intel_CPUs,new_Intel_CPUs$Vertical_Segment=="Embedded")
qqnorm(Embedded$log_TDP)
qqline(Embedded$log_TDP)
shapiro.test(Embedded$log_TDP)

#Kiểm tra tính đồng nhất các phương sai
bartlett.test(log_TDP~as.factor(Vertical_Segment), data = new_Intel_CPUs)

#phan tich anova
anova_model_1 <- aov(log_TDP~Vertical_Segment,data=new_Intel_CPUs)
summary(anova_model_1)

#thuc hien so sanh boi
TukeyHSD(anova_model_1)

