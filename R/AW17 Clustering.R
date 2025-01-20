# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cluster)
library(factoextra)
library(tidyverse)
library(Rtsne)
library(ggpubr)

# Load datasets
Ventas_Totales <- read_excel("C:/Users/manus/DesktopTareas/dataset_AW.xlsx", 
                             sheet = "ST Ventas Totales ")
Clientes <- read_excel("C:/Users/manus/Desktop/Tareas/dataset_AW.xlsx", 
                       sheet = "Var Discreta Adq Bicicleta")
Productos <- read_excel("C:/Users/manus/Desktop/Tareas/dataset_AW.xlsx", 
                        sheet = "Datos sin etiquetas (No Supervs")

# Initial dataset inspection
str(Ventas_Totales)
str(Clientes)
str(Productos)

summary(Ventas_Totales)
summary(Clientes)
summary(Productos)

# Handle NULL values in certain categories
Productos <- Productos %>%
  mutate(Size = na_if(Size, "NULL"),
         Weight = na_if(Weight, "NULL"),
         Color = na_if(Color, "NULL"))

sum(is.na(Ventas_Totales))
sum(is.na(Clientes))
sum(is.na(Productos))

# Decision: Keep NULL values for Size and Weight, as these variables won't be heavily used

#### 1. Which country (Country) generates the highest revenue (TotalAmount)? ####
# Use the "Var Discreta Adq Bicicleta" sheet and create a TOP 10 chart

Top_10 <- Clientes %>%
  group_by(Country) %>%
  summarise(Total_Ventas = sum(TotalAmount, na.rm = TRUE)) %>%
  arrange(desc(Total_Ventas)) %>%
  slice_head(n = 10) 

ggplot(Top_10, aes(x = reorder(Country, Total_Ventas), y = Total_Ventas, fill = Country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Total Sales",
    x = "Country",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#### 2. Analyze sales trends using the "ST Ventas Totales" sheet ####

# Online sales are summed on specific dates, creating spikes. We'll aggregate sales weekly to analyze trends.

ventas_semanales <- Ventas_Totales %>%
  mutate(Semana = format(OrderDate, "%Y-%U")) %>%
  group_by(Semana) %>%
  summarise(Ventas_Totales = sum(Sales, na.rm = TRUE))

ggplot(ventas_semanales, aes(x = Semana, y = Ventas_Totales, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  labs(
    title = "Weekly Sales Trends",
    x = "Week",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# To smooth out spikes, group sales from the 15th of one month to the 14th of the next
ventas_diarias <- Ventas_Totales %>%
  mutate(
    Periodo = case_when(
      day(OrderDate) >= 15 ~ paste0(year(OrderDate), "-", month(OrderDate), " (15-14)"),
      TRUE ~ paste0(year(OrderDate) - (month(OrderDate) == 1), "-", ifelse(month(OrderDate) == 1, 12, month(OrderDate) - 1), " (15-14)")
    )
  )

ventas_por_periodo <- ventas_diarias %>%
  group_by(Periodo) %>%
  summarise(Ventas_Totales = sum(Sales, na.rm = TRUE)) %>%
  ungroup()

ggplot(ventas_por_periodo, aes(x = Periodo, y = Ventas_Totales, group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Sales Trends (15th to 14th of the Next Month)",
    x = "Period",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### 3. Average price of different product categories and price differences based on color ####

# Calculate average price by category
PrecioMedio_Productos <- Productos %>%
  group_by(Name) %>%
  summarise(PrecioMedio = mean(UnitPrice, na.rm = TRUE)) %>%
  arrange(desc(PrecioMedio))

ggplot(PrecioMedio_Productos, aes(x = reorder(Name, -PrecioMedio), y = PrecioMedio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Price by Category",
    x = "Category",
    y = "Average Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analyze price differences by color
datos_limpios <- Productos %>%
  filter(!is.na(Color))

media_color_categoria <- datos_limpios %>%
  group_by(Name, Color) %>%
  summarise(
    Media_Color = mean(UnitPrice, na.rm = TRUE),
    Desviación_Color = sd(UnitPrice, na.rm = TRUE),
    Cuenta = n()
  )

comparativa <- media_color_categoria %>%
  left_join(PrecioMedio_Productos, by = "Name") %>%
  mutate(Diferencia = Media_Color - PrecioMedio)

ggplot(comparativa, aes(x = Name, y = Diferencia, fill = Color)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Price Difference by Color (Compared to Category Average)",
    x = "",
    y = "Price Difference"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Black" = "black", "Silver" = "grey", "Red" = "red", "Blue" = "blue", "Multi" = "purple"))


# We can see that, in general, colors do not significantly affect the price, although there are exceptions, 
# such as the Road Bikes category, which is influenced by color.
# The color red significantly increases the price, while darker colors like black or silver decrease the price of these products.

###### 4. What are the characteristics of bicycle buyers? 
# To answer this, use the variable BikePurchase (1 means the customer is a bicycle buyer).

# Since we want to identify the characteristics of buyers, filter the data to include only buyers.

Buyers <- Clientes %>%
  filter(BikePurchase == 1)

# Encoding categorical variables

buyers_clean <- Buyers %>%
  select(Age, Group, YearlyIncome, Gender, MaritalStatus, HomeOwnerFlag, Education, Occupation, TotalChildren, NumberCarsOwned)

buyers_clean$Group <- as.factor(buyers_clean$Group)
buyers_clean$YearlyIncome <- as.factor(buyers_clean$YearlyIncome)
buyers_clean$Gender <- as.factor(buyers_clean$Gender)
buyers_clean$MaritalStatus <- as.factor(buyers_clean$MaritalStatus)
buyers_clean$Education <- as.factor(buyers_clean$Education)
buyers_clean$Occupation <- as.factor(buyers_clean$Occupation)

# Calculate Gower distances

gower_df <- daisy(buyers_clean,
                  metric = "gower")

summary(gower_df)

# Calculate silhouette width to determine the optimal number of clusters

silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE, k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette, xlab = "Clusters",
     ylab = "Silhouette Width") 

lines(1:10, silhouette)

# To visualize the silhouette in ggplot

silggplot <- data.frame(silhouette)
silggplot$n <- c(1,2,3,4,5,6,7,8,9,10)

silggplot[1,1] <- 0

ggplot(silggplot, aes(x = n, y = silhouette)) +
  geom_line(color="#1f618d")+
  geom_point(color="#1f618d")+
  scale_x_continuous(breaks = seq(1, 10, by = 1))+
  theme_classic()+
  labs(y="Average silhouette width", x = "Number of clusters k", title="Optimal number of clusters")

# Estimate clusters and assign a cluster to each group
# Create 2 clusters

pam_buyers = pam(gower_df, diss = TRUE, k = 2)

# To summarize the information for each cluster

pam_summary_buyers <- buyers_clean %>%
  mutate(cluster = pam_buyers$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))

pam_summary_buyers$cluster_summary[[2]]

# To represent it graphically

tsne_object <- Rtsne(gower_df, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_buyers$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster)) +
  scale_color_manual(values=c("#0098cd", "#7fb433"))

# To add the cluster to the original dataset

final_buyers_df <- bind_cols(Buyers, pam_buyers['clustering'])
final_buyers_df$clustering <- as.factor(final_buyers_df$clustering)

# Separate into clusters

cluster1 <- filter(final_buyers_df, clustering == 1)
cluster2 <- filter(final_buyers_df, clustering == 2)

# To recreate the initial plot but for each cluster

age <- ggplot(final_buyers_df, aes(x = Age, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) +
  labs(y = "", x = "Age")

group <- ggplot(final_buyers_df, aes(x = Group, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) + 
  labs(y = "", x = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

education <- ggplot(final_buyers_df, aes(x = Education, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) + 
  labs(y = "", x = "Education") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

occupation <- ggplot(final_buyers_df, aes(x = Occupation, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) +
  labs(y = "", x = "Occupation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

marital <- ggplot(final_buyers_df, aes(x = MaritalStatus, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) +
  labs(y = "", x = "Marital Status")

owner <- ggplot(final_buyers_df, aes(x = HomeOwnerFlag, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) +
  labs(y = "", x = "Home Owner Flag")

cars <- ggplot(final_buyers_df, aes(x = NumberCarsOwned, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) +
  labs(y = "", x = "Number Cars Owned")

total_children <- ggplot(final_buyers_df, aes(x = TotalChildren, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) +
  labs(y = "", x = "Total Children")

gender <- ggplot(final_buyers_df, aes(x = Gender, fill = clustering)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#0098cd", "#7fb433")) +
  labs(y = "", x = "Gender")

ggarrange(age, group, education, gender, occupation, marital, owner, cars, total_children,
          labels = c(""),
          ncol = 3, nrow = 3)

# Now, without separating into clusters

age <- ggplot(final_buyers_df, aes(x = Age)) + 
  geom_bar(fill = "#0098cd") +
  labs(y = "", x = "Age")

group <- ggplot(final_buyers_df, aes(x = Group)) + 
  geom_bar(fill = "#0098cd") + 
  labs(y = "", x = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

education <- ggplot(final_buyers_df, aes(x = Education)) + 
  geom_bar(fill = "#0098cd") + 
  labs(y = "", x = "Education") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

occupation <- ggplot(final_buyers_df, aes(x = Occupation)) + 
  geom_bar(fill = "#0098cd") +
  labs(y = "", x = "Occupation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

marital <- ggplot(final_buyers_df, aes(x = MaritalStatus)) + 
  geom_bar(fill = "#0098cd") +
  labs(y = "", x = "Marital Status")

owner <- ggplot(final_buyers_df, aes(x = HomeOwnerFlag)) + 
  geom_bar(fill = "#0098cd") +
  labs(y = "", x = "Home Owner Flag")

cars <- ggplot(final_buyers_df, aes(x = NumberCarsOwned)) + 
  geom_bar(fill = "#0098cd") +
  labs(y = "", x = "Number Cars Owned")

total_children <- ggplot(final_buyers_df, aes(x = TotalChildren)) + 
  geom_bar(fill = "#0098cd") +
  labs(y = "", x = "Total Children")

gender <- ggplot(final_buyers_df, aes(x = Gender)) + 
  geom_bar(fill = "#0098cd") +
  labs(y = "", x = "Gender")

ggarrange(age, group, education, gender, occupation, marital, owner, cars, total_children,
          labels = c(""),
          ncol = 3, nrow = 3)


