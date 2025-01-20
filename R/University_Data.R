library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)

matriculas_grado <- matriculas_gradoposgrado
matriculas_master <- read_excel("C:/Users/Manu/Desktop/matriculas_gradoposgrado.xlsx", 
                                sheet = "Matriculados Master")

#### 1. What types of variables are in the dataset? Create two summary tables (one for Bachelor's and one for Master's) showing, for each university, the maximum, minimum, mean, and standard deviation of the number of enrolled students and the percentage of women in their studies for the 2021/2022 academic year. Briefly describe the most relevant findings from this initial knowledge. ####

# Table for BACHELOR'S degree
resumen_grado <- matriculas_grado %>%
  group_by(Universidad)%>%
  summarise(Max_matriculas = max(matriculas2122, na.rm = TRUE),
            Min_matriculas = min(matriculas2122, na.rm = TRUE),
            Mean_matriculas = mean(matriculas2122, na.rm = TRUE),
            Desv_matriculas = sd(matriculas2122, na.rm = TRUE),
            Max_mujeres = max(mujeres2122, na.rm = TRUE),
            Min_mujeres = min(mujeres2122, na.rm = TRUE),
            Mean_mujeres = mean(mujeres2122, na.rm = TRUE),
            Desv_mujeres = sd(mujeres2122, na.rm = TRUE))
  
# Table for MASTER'S degree
resumen_master <- matriculas_master %>%
  group_by(Universidad) %>%
  summarise(Max_matriculas = max(matriculas2122, na.rm = TRUE),
            Min_matriculas = min(matriculas2122, na.rm = TRUE),
            Mean_matriculas = mean(matriculas2122, na.rm = TRUE),
            Desv_matriculas = sd(matriculas2122, na.rm = TRUE),
            Max_mujeres = max(mujeres2122, na.rm = TRUE),
            Min_mujeres = min(mujeres2122, na.rm = TRUE),
            Mean_mujeres = mean(mujeres2122, na.rm = TRUE),
            Desv_mujeres = sd(mujeres2122, na.rm = TRUE))

View(resumen_grado)
view(resumen_master)

#### 2. Graphical representation for Bachelor's and Master's degrees of the Top 30 universities with the most enrolled students across the 7 academic years in the dataset. Is there alignment between the Top 30 for Bachelor's and Master's? ####

# Total enrollments for Bachelor's by University (TOP 30)

Top_30_grado <- matriculas_grado %>%
  mutate(total_matriculados = matriculas2122+matriculas2021+matriculas1920+
           matriculas1819+matriculas1718+matriculas1617+matriculas1516) %>%
  group_by(Universidad) %>%
  summarise(total_matriculados = sum(total_matriculados, na.rm = TRUE)) %>%
  arrange(desc(total_matriculados)) %>%
  slice(1:30)

# Total enrollments for Master's by University (TOP 30)

Top_30_master <- matriculas_master %>%
  mutate(total_matriculados = matriculas2122+matriculas2021+matriculas1920+
           matriculas1819+matriculas1718+matriculas1617+matriculas1516) %>%
  group_by(Universidad) %>%
  summarise(total_matriculados = sum(total_matriculados, na.rm = TRUE)) %>%
  arrange(desc(total_matriculados)) %>%
  slice(1:30)

# Graphical representation
library(ggplot2)

#Bachelor's
Top_30_grado %>%
  ggplot(aes(x=total_matriculados, y=reorder(Universidad, total_matriculados)))+
  geom_col(fill="#07469c", width = .6)+
  labs(y="Universidad",
       x="Total Matriculados",
       title = "Total de Alumnos Matriculados en Grados (2015-2022)",
       subtitle = "Top30 Universidades Españolas")+
  theme_minimal()

#Master's
Top_30_master %>%
  ggplot(aes(x=total_matriculados, y=reorder(Universidad, total_matriculados)))+
  geom_col(fill="#bb2409", width = .6)+
  labs(y="Universidad",
       x="Total Matriculados",
       title = "Total de Alumnos Matriculados en Máster (2015-2022)",
       subtitle = "Top30 Universidades Españolas")+
  theme_minimal()

# Check for alignment between the Top 30 Bachelor's and Master's
universidades_grado <- Top_30_grado$Universidad
universidades_master <- Top_30_master$Universidad

# Intersection
universidades_comunes <- intersect(universidades_grado, universidades_master)

# Show the common universities
View(universidades_comunes)

universidades_comunes_df <- data.frame(Universidades = universidades_comunes)

# View the table
View(universidades_comunes_df)


#### 3. Table and graphical representation of the number of students and percentage of women in Master's programs in "Social and Legal Sciences" that contain the terms "Business Intelligence", "Business Analytics", "Data Analysis", or "Data Science" in their title. ####
library(tidyverse)

# Filter Master's programs in Social and Legal Sciences that contain the key terms

masters_ciencias_s_j <- matriculas_master %>%
  filter(`Rama` == "Ciencias Sociales y Jurídicas", 
         str_detect(Titulación, 
                    "Inteligencia de Negocio|Analítica de Negocio|Análisis de Datos|Ciencia de Datos"))

resumen_ciencias_ssjj <- masters_ciencias_s_j %>%
  group_by(Titulación) %>%
  summarise(
    total_alumnos = sum(matriculas2122, na.rm = TRUE),
    porcentaje_mujeres = mean(mujeres2122, na.rm = TRUE))

# Modify the names of Master's degrees with abbreviations
resumen_ciencias_ssjj <- resumen_ciencias_ssjj %>%
  mutate(Titulación = str_replace_all(Titulación, c(
    "Análisis de Datos Masivos en Economía y Empresa por la Universitat de les Illes Balears" = "Análisis de Datos Masivos en Eco. y Empresa - UIB",
    "Análisis de Datos para la Inteligencia de Negocios por la Universidad de Oviedo" = "Análisis de Datos para B.I. - UO",
    "Análisis de Datos para los Negocios / Master of Science in Business Analytics por la Universidad Ramón Llull" = "Análisis de Datos para los Neg. / MSc Business Analytics - URL",
    "Analítica de Negocio y Manejo de Datos / Business Analytics and Big Data por la IE Universidad" = "Analítica de Neg. y Manejo de Datos / Business Analytics & Big Data - IE",
    "Ciencia de Datos por la Universidad Autónoma de Barcelona y la Universidad Pompeu Fabra" = "Ciencia de Datos - UAB y UPF",
    "Inteligencia de Negocio por la Universidad Internacional de La Rioja" = "Inteligencia de Negocio - UNIR",
    "Minería de Datos e Inteligencia de Negocios por la Universidad Complutense de Madrid" = "Minería de Datos e I. de Negocios - UCM",
    "Modelización y Análisis de Datos Económicos por la Universidad de Castilla La Mancha" = "Modelización y Análisis de Datos Económicos - UCLM"
  )))


# Graphical representation
resumen_ciencias_ssjj %>%
  ggplot(aes(x = reorder(Titulación, total_alumnos), y = total_alumnos, fill = Titulación))+
  geom_bar(fill="#0098cd", stat = "identity") +
  coord_flip() +
  labs(title = "Nº Alumnos por Titulación en Másteres de CCSSJJ (2021/2022)",
       x = "Titulación", y = "Total de Alumnos")+
  theme(legend.position = "none")

#### 4. Top 30 degrees with the highest percentage of women. Additionally, the general management wants to know if the percentage of women in each "Field of Knowledge" has changed over the last 7 academic years. ###

# Find the degrees with the highest percentage of women


matriculas_master_pmuj <- mutate(matriculas_master, mujeres = matriculas2122*mujeres2122)



top_30_mujeres_master <- matriculas_master_pmuj %>%
  arrange(desc(`mujeres`)) %>%
  slice(1:30)


# Show the Top 30 table

print(top_30_mujeres_master)

# Graph the Top 30


ggplot(top_30_mujeres_master, aes(x = reorder(Titulación, `mujeres`), y = `mujeres`)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 30 Titulaciones con mayor presencia de Mujeres",
       x = "Titulación", y = "Presencia de mujeres")

# Group by field and calculate the average percentage of women per year
agrupar_mujeres <- matriculas_master %>%
  group_by(Rama) %>%
  summarise_at(vars(starts_with("mujeres")), mean, na.rm = TRUE)

# Transpose for easier visualization
tendencia_mujeres <- gather(agrupar_mujeres, key = "Año", value = "Porcentaje_Mujeres", -Rama)

# Graph the trend
ggplot(tendencia_mujeres, aes(x = Año, y = Porcentaje_Mujeres, color = Rama)) +
  geom_point(size = 3) 
labs(title = "Tendencia del Porcentaje de Mujeres por Rama de Conocimiento (2015-2022)",
     x = "Año", y = "Porcentaje de Mujeres") +
  theme_minimal()

### Final reflection on the usefulness of the data:

# In this case, the data provides useful analysis for decision-making within the educational group, as it allows for:

# Identifying the current demand and trends in Master's programs related to Business Analytics in Social and Legal Sciences.
# Seeing which degrees have the highest female participation, which is important for designing targeted marketing campaigns for this segment.
# Observing gender trends in different fields of knowledge, allowing for adjustments in educational strategies to promote inclusion and diversity.


ggplot(top_30_mujeres_master, aes(x = reorder(Titulación, `mujeres`), y = `mujeres`)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 30 Titulaciones con mayor presencia de Mujeres",
       x = "Titulación", y = "Presencia de mujeres") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 8),  # Ajusta el tamaño del texto en el eje y
    plot.margin = margin(t = 10, r = 10, b = 10, l = 20)  # Añade margen izquierdo
  )


