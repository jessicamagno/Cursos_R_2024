########################################################
####### LAINFS - UFPR ##################################
########################## GGPLOT2 #####################
# Por Jessica M. Magno
# Script utilizado na aula do dia 03/06/2024

#----------------------------------------- Parte teorica

# Instalacao dos pacotes necessarios
install.packages("ggplot2")
install.packages("vcd")
install.packages("scales")
install.packages("dplyr")

# Carregando bibliotecas necessarias
library(ggplot2)

data(ChickWeight)
head(ChickWeight)

ggplot(data = ChickWeight, aes(x = Time, y = weight))

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point(aes(color = Diet))

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point(aes(color = Diet)) +
  coord_cartesian() +
  theme_classic()

#----------------------------------------- Parte Pratica

# BAR CHARTS

data(Arthritis, package="vcd")
head(Arthritis)
table(Arthritis$Improved)

# Bar plot simples

ggplot(data = Arthritis, aes(x=Improved)) + geom_bar() +
  labs(title="Simple Bar chart",
       x="Improvement",
       y="Frequency")

# Deixando o plot na horizontal
ggplot(Arthritis, aes(x=Improved)) + geom_bar() +
  labs(title="Horizontal Bar chart",
       x="Improvement",
       y="Frequency") +
  coord_flip()

# Graficos de barras empilhados, agrupados e preenchidos

table(Arthritis$Improved, Arthritis$Treatment)

ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "stack") +
  labs(title="Stacked Bar chart",
       x="Treatment",
       y="Frequency")

ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "dodge") +
  labs(title="Grouped Bar chart",
       x="Treatment",
       y="Frequency")

ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "fill") +
  labs(title="Filled Bar chart",
       x="Treatment",
       y="Proportion")

# Ajustes gerais em graficos de barra

# Cores
ggplot(Arthritis, aes(x=Improved)) +
  geom_bar(fill=c("red", "grey", "gold"), color="black") +
  labs(title="Treatment Outcome")

ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "stack", color="black") +
  scale_fill_manual(values=c("red", "grey", "gold")) +
  labs(title="Stacked Bar chart",
       x="Treatment",
       y="Frequency")

# Labels
head(mpg)

ggplot(mpg, aes(x=model)) +
  geom_bar() +
  labs(title="Car models in the mpg dataset",
       y="Frequency", x="")

ggplot(mpg, aes(x=model)) +
  geom_bar() +
  labs(title="Car models in the mpg dataset",
       y="Frequency", x="") +
  coord_flip()

ggplot(mpg, aes(x=model)) +
  geom_bar() +
  labs(title="Model names in the mpg dataset",
       y="Frequency", x="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))

#---------------------

# HISTOGRAMAS

library(scales)
data(mpg)
cars2008 <- mpg[mpg$year == 2008, ]

ggplot(cars2008, aes(x=cty)) +
  geom_histogram() +
  labs(title="Default histogram")

ggplot(cars2008, aes(x=cty)) +
  geom_histogram(bins=20, color="white", fill="steelblue") +
  labs(title="Colored histogram with 20 bins",
       x="City Miles Per Gallon",
       y="Frequency")

ggplot(cars2008, aes(x=cty, after_stat(density))) +
  geom_histogram(bins=20, color="white", fill="steelblue") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Histogram with percentages",
       y= "Percent",
       x="City Miles Per Gallon")

ggplot(cars2008, aes(x=cty, after_stat(density))) +
  geom_histogram(bins=20, color="white", fill="steelblue") +
  scale_y_continuous(labels=scales::percent) +
  geom_density(color="red", linewidth=1) +
  labs(title="Histogram with density curve",
       y="Percent" ,
       x="Highway Miles Per Gallon")

#-------------------

# BOX PLOTS

cars <- mpg[mpg$cyl != 5, ]
cars$Cylinders <- factor(cars$cyl)
cars$Year <- factor(cars$year)

ggplot(cars, aes(x=Cylinders, y=cty)) +
  geom_boxplot() +
  labs(x="Number of Cylinders",
       y="Miles Per Gallon",
       title="Car Mileage Data")

ggplot(cars, aes(x=Cylinders, y=cty)) +
  geom_boxplot(notch=TRUE,
               fill="steelblue",
               varwidth=TRUE) +
  labs(x="Number of Cylinders",
       y="Miles Per Gallon",
       title="Car Mileage Data")

ggplot(cars, aes(x=Cylinders, y=cty, fill = Year)) +
  geom_boxplot() +
  labs(x="Number of Cylinders",
       y="Miles Per Gallon",
       title="City Mileage by # Cylinders and Year") +
  scale_fill_manual(values=c("gold", "green"))

#----------------------

# VIOLIN PLOTS

cars <- mpg[mpg$cyl != 5, ]
cars$Cylinders <- factor(cars$cyl)

ggplot(cars, aes(x=Cylinders, y=cty)) +
  geom_boxplot(width=0.2,
               fill="green") +
  geom_violin(fill="gold",
              alpha=0.3) +
  labs(x="Number of Cylinders",
       y="City Miles Per Gallon",
       title="Violin Plots of Miles Per Gallon")

#---------------------

# DOT PLOTS

library(dplyr)

plotdata <- mpg %>%
  filter(year == "2008") %>%
  group_by(model) %>%
  summarize(meanHwy=mean(hwy))

plotdata

ggplot(plotdata, aes(x=meanHwy, y=model)) +
  geom_point() +
  labs(x="Miles Per Gallon",
       y="",
       title="Gas Mileage for Car Models")

ggplot(plotdata, aes(x=meanHwy, y=reorder(model, meanHwy))) +
  geom_point() +
  labs(x="Miles Per Gallon",
       y="",
       title="Gas Mileage for Car Models")

#-------------------------

