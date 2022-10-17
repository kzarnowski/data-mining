# 1
lista_obecnosci <- c(25, 30, 16, 20, 10, 21, 12)
names(lista_obecnosci) <- c("Pn", "Wt", "Sr", "Cz", "Pt", "S", "N")
lista_obecnosci[c("Pn", "Pt")]
max(lista_obecnosci)
lista_obecnosci[which.max(lista_obecnosci)]
lista_obecnosci[which.min(lista_obecnosci)]
mean(lista_obecnosci)
sum(lista_obecnosci >= 20)
liczebnosc_grup <- c(25, 30, 18, 20, 15, 21, 15)

lista_obecnosci == liczebnosc_grup
lista_obecnosci < 0.5 * liczebnosc_grup

#2
US <- c(460.998, 290.475, 309.306)
nonUS <- c(314.4, 247.9, 165.8)

A <- matrix(cbind(US, nonUS), ncol=2)
colnames(A) <- c("US", "non-US")
rownames(A) <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")
A

A[nrow(A),]
A[1,2]

#3

planets <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- rep(c("Terrestrial planet", "Gas giant"), each=4)
diameter <- c(0.382, 0.949, 1.000, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1.00, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- rep(c(FALSE, TRUE), each=4)

df <- data.frame(
  name = planets,
  type = type,
  diameter = diameter,
  rotation = rotation,
  rings = rings
)

df[which(df$name == 'Mars'), ]$diameter
df[which(df$name == 'Uranus'), ]
str(df)
summary(df$rings)

planet_rings <- df[df$rings == TRUE,]
small_planets <- df[df$diameter < 1, ]

#4
colors <- factor(
  c("Red", "Green", "Blue", "Green", "Blue", "Blue", "Blue")
)
colors

speed <- factor(
  c("Very slow", "Slow", "Medium", "Slow", "Slow", "Fast", "Very fast", "Fast", "Very slow" ),
  levels= c("Very slow", "Slow", "Medium", "Fast", "Very fast"),
  ordered=TRUE)
speed

str(speed)
table(speed)

#5
ocen <- function(liczba_obecnosci, kolokwium, kartkowka, aktywnosc) {
  max_obecnosci <- 15
  max_ocena <- 100
  if (liczba_obecnosci < 0.5 * max_obecnosci) {
    return("Brak zaliczenia")
  }
  
  if (kolokwium < 0.5 * max_ocena){
    return(2.0)
  }
  
  punkty <- 0.9 * kolokwium + 0.05 * kartkowka + 0.05 * aktywnosc
  
  if(punkty < 50) return(2.0)
  if(punkty < 61) return(3.0)
  if(punkty < 71) return(3.5)
  if(punkty < 81) return(4.0)
  if(punkty < 91) return(4.5)
  return(5.0)
}

ocen(8, 95, 60, 60)

#6
library(gapminder)
library(dplyr)
gapminder %>% 
  filter(country == 'China', year == 2002)

gapminder %>% 
  filter(year == 1957) %>% arrange(pop)

gapminder2007 <- gapminder %>% filter(year == 2007)
gapminder2007["lifeExpMonths"] = gapminder2007["lifeExp"] * 12
gapminder2007 <- gapminder2007 %>% arrange(lifeExpMonths)

gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp=median(lifeExp), maxGdpPercap=mean(gdpPercap))

gapminder %>%
  group_by(continent, year) %>%
  summarize(medianLifeExp=median(lifeExp), maxGdpPercap=mean(gdpPercap))

#7
library(readxl)
cars <- read.table("cars.txt", sep=",")
str(cars)
head(cars)

koty_ptaki <- read.csv("koty_ptaki.csv", sep=";", dec=",")
str(koty_ptaki)
head(koty_ptaki)

ludn <- read_excel("LUDN_2137_20160225144358.xlsx", sheet="DATA")
str(ludn)
head(ludn)

write.csv(cars, "cars_export.csv", row.names = FALSE)
write.csv(koty_ptaki, "koty_ptaki_export.csv", row.names = FALSE)
write.csv(ludn, "ludn_export.csv", row.names = FALSE)

#8
library(ggplot2)

hist(koty_ptaki$dlugosc, breaks = 10)
ggplot(koty_ptaki, aes(x=dlugosc)) +
  geom_histogram(bins=10)

plot(koty_ptaki$dlugosc, koty_ptaki$waga)
ggplot(koty_ptaki, aes(x=dlugosc, y=waga)) +
  geom_point()

plot(koty_ptaki$waga, koty_ptaki$predkosc, type="l")
ggplot(koty_ptaki, aes(x=waga, y=predkosc)) +
  geom_line()

boxplot(predkosc ~ druzyna, data=koty_ptaki)
ggplot(koty_ptaki, aes(x=druzyna, y=predkosc)) +
  geom_boxplot()

