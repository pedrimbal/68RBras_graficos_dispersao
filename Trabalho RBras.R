#install.packages("copula")
#install.packages("gridExtra")
#install.packages("ggplot2")
library(copula)
library(ggplot2)
library(gridExtra)
#pdf("Gráfico de dispersão parábola.pdf")
# Parábola
x_parabola <- seq(-10, 10, length.out = 1000)  # Valores de x
y_parabola <- x_parabola^2  # Valores de y (parábola)

# Adicionar ruído aos pontos para dispersão
x_al <- seq(-10, 10, length.out = 100)  # Valores de x aleatórios 
y_al <- x_al^2 + runif(length(x_al), -12, 12)  # Valores de y aleatórios na parábola

# Gráfico de dispersão de uma parábola 
plot(x_parabola, y_parabola, type = "l", col = "red", lwd = 2,  
     xlab = "Eixo X", ylab = "Eixo Y")  

# Adicionar pontos de dispersão que formam a parábola
points(x_al, y_al, col = "blue", pch = 16)
mtext("Figura 2: Gráfico de dispersão parábola", side = 1,
      line = 4, adj = 0, cex = 1, font = 2)
#dev.off()
#pdf("Gráfico de dispersão linear.pdf")
set.seed(123)  
x <- rnorm(100, mean = 5, sd = 2) #média 5 e desvio padrão 2
y <- 2 * x + rnorm(100, mean = 0, sd = 1.5)  # Gerar valores de y linearmente relacionados a x 

# Gráfico de dispersão linear
plot(x, y, col = "blue", pch = 16, 
     xlab = "Eixo X", ylab = "Eixo Y") 
mtext("Figura 1: Gráfico de dispersão linear", side = 1,
      line = 4, adj = 0, cex = 1, font = 2)

# Adicionar linha de regressão linear
abline(lm(y ~ x), col = "red", lwd = 2)  
#dev.off()
set.seed(123) 

# Gerar dados para os pontos centrais
x_central <- rnorm(1000, mean = 5, sd = 0.5)  # 1000 valores aleatórios em torno de 5 com desvio padrão 1
y_central <- rnorm(1000, mean = 5, sd = 0.5)  # 1000 valores aleatórios em torno de 5 com desvio padrão 1

# Adicionar outliers
x_outliers <- c(2, 8, 3)  # Valores x dos outliers
y_outliers <- c(10, 1, 9)  # Valores y dos outliers

# Gráfico de dispersão Circular
plot(x_central, y_central, col = "blue", pch = 16,   
     xlim = c(0, 10), ylim = c(0, 10),  # Limites dos eixos x e y
     xlab = "Eixo X", ylab = "Eixo Y")  
mtext("Figura 3: Gráfico de dispersão circular", side = 1,
      line = 4, adj = 0, cex = 1, font = 2)
points(x_outliers, y_outliers, col = "blue", pch = 16) 

#pdf("Gráfico de dispersão exponencial.pdf")
set.seed(123) 
x_exp <- seq(1, 10, by = 0.1)  # Valores de x de 1 a 10 em incrementos de 0.1
y_exp <- exp(x_exp) + rnorm(length(x_exp), mean = 0, sd = 1)  # Valores de y exponenciais 

# Gráfico de dispersão exponencial
plot(x_exp, y_exp, col = "blue", pch = 16,  
     xlab = "Eixo X", ylab = "Eixo Y") 
mtext("Figura 4: Gráfico de dispersão exponencial", side = 1,
      line = 4, adj = 0, cex = 1, font = 2)
#dev.off()
# Dados
x <- c(0.90, 0.92, 1.48, 3.15, 2.86, 0.73, 1.85, 2.49, 0.70, 1.43, 1.91, 1.45, 0.76, 2.29, 0.92, 2.44, 2.32, 0.94)
y <- c(8, 5, 3, 1, 2, 7, 3, 1, 8, 4, 2, 5, 9, 2, 6, 2, 1, 7)

# Ajuste do modelo de regressão quadrática
modelo <- lm(y ~ poly(x, 2, raw = TRUE))

# Preparar dados para plotagem da parábola de ajuste
x_valores <- seq(min(x), max(x), length.out = 100)
y_valores <- predict(modelo, newdata = data.frame(x = x_valores))

# Criar o gráfico com ggplot
grafico <- ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3) +  # pontos de dados
  geom_line(data = data.frame(x = x_valores, y = y_valores), aes(x = x, y = y), color = "red", size = 1.5) +  # parábola de ajuste
  labs(x = "x", y = "y")  # rótulos dos eixos

# Mostrar o gráfico
print(grafico)
#pdf("Gráfico de dispersão com relação caudal.pdf")
set.seed(812112)
n_caudal<-100
b0<-2
u1<-runif(n_caudal,0,11)
x_caudal<-vector()
for(i in 1:n_caudal){
  if(u1[i]<8){
    x_caudal[i]<-runif(1,0,3)
  }
  if(u1[i]>8){
    x_caudal[i]<-runif(1,3,17)
  }
}
e_caudal<-rnorm(n_caudal,0,2)
y_caudal<-b0+x_caudal+e_caudal
plot(x_caudal,y_caudal,xlab="Eixo X",ylab="Eixo Y",pch=16,col="blue")
mtext("Figura 5: Gráfico de dispersão com relação caudal", side = 1,
      line = 4, adj = 0, cex = 1, font = 2)
#dev.off()

#pdf("Gráfico de dispersão para duas variáveis independêntes.pdf")
set.seed(123)
e1<-rexp(100,10)
e2<-rexp(100,5)
plot(e1,e2,xlab="Exponencial com média 0.1",ylab="exponencial com média 0.2",pch=16,col="blue")
mtext("Figura 6: Gráfico de dispersão para duas variáveis independêntes", side = 1,
      line = 4, adj = 0, cex = 1, font = 2)
#dev.off()

# teste normalização
mu_1<-mean(e1)
sd_1<-sd(e1)
novo_e1<-(e1-mu_1)/sd_1
mu_2<-mean(e2)
sd_2<-sd(e2)
novo_e2<-(e2-mu_2)/sd_2
plot(novo_e1,novo_e2)
p1 <- ggplot() +
  geom_point(aes(x = novo_e1,y=novo_e2), color = "blue", shape = 16) +
  labs(x = "Eixo X", y = "Eixo Y")
p1

#pdf("Gráfico de dispersão para duas variáveis independêntes após transformações.pdf")
u<-1-exp(-10*e1)
v<-1-exp(-5*e2)
plot(u,v,xlab="Exponencial com média 0.1 após transformação",ylab="exponencial com média 0.2 após transformação",pch=16,col="blue")
mtext("Figura 7: Gráfico de dispersão para duas variáveis independêntes", side = 1,
      line = 4, adj = 0, cex = 1, font = 2)
#dev.off()

# Gráfico da parábola
p1 <- ggplot() +
  geom_line(aes(x = x_parabola, y = y_parabola), color = "red", size = 2) +
  geom_point(aes(x = x_al, y = y_al), color = "blue", shape = 16) +
  labs(x = "Eixo X", y = "Eixo Y", title = "Gráfico de dispersão parábola")

# Gráfico linear
x <- rnorm(100, mean = 5, sd = 2) #média 5 e desvio padrão 2
y <- 2 * x + rnorm(100, mean = 0, sd = 1.5)
p2 <- ggplot() +
  geom_point(aes(x = x, y = y), color = "blue", shape = 16) +
  geom_smooth(aes(x = x, y = y), method = "lm", color = "red", se = FALSE) +
  labs(x = "Eixo X", y = "Eixo Y", title = "Gráfico de dispersão linear")

# Juntar os gráficos
grid.arrange(p2,p1, nrow = 1)

# Gráfico de dispersão circular
p3 <- ggplot() +
  geom_point(aes(x = x_central, y = y_central), color = "blue", shape = 16) +
  geom_point(aes(x = x_outliers, y = y_outliers), color = "blue", shape = 16) +
  xlim(0, 10) + ylim(0, 10) +
  labs(x = "Eixo X", y = "Eixo Y")

# Gráfico de dispersão exponencial
p4 <- ggplot() +
  geom_point(aes(x = x_exp, y = y_exp), color = "blue", shape = 16) +
  labs(x = "Eixo X", y = "Eixo Y", title = "Gráfico de dispersão exponencial")

p7 <- ggplot()+ geom_point(aes(x=x_caudal,y=y_caudal),color="blue",shape=16)+
  labs(x="Eixo X",y="Eixo Y", title="Gráfico de dispersão linear caudal")

p3

# Gráfico de dispersão para duas variáveis independentes
p5 <- ggplot() +
  geom_point(aes(x = e1, y = e2), color = "blue", shape = 16) +
  labs(x = "Exponencial com média 0.1", y = "Exponencial com média 0.2", title = "Gráfico de dispersão para duas variáveis independentes")

# Gráfico de dispersão para duas variáveis independentes após transformações
p6 <- ggplot() +
  geom_point(aes(x = u, y = v), color = "blue", shape = 16) +
  labs(x = "Exponencial com média 0.1 após transformação", y = "Exponencial com média 0.2 após transformação", title = "Gráfico de dispersão para duas variáveis independentes após transformações")

# Juntar os gráficos
grid.arrange(p5, p6, nrow = 1)

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)


set.seed(123)
v<-runif(17,0,2)
w<-runif(17,0,0.3)
x_antes<-c(0,78,56,28,16,16,13.5,13,7,7,6,4.5,2,v)
y_antes<-c(0,24,32,4,2,6,3,9,0,1,4.7,2,2,w)
p8 <- ggplot() +
  geom_point(aes(x = x_antes, y = y_antes), color = "blue", shape = 16) +
  labs(x="Eixo X",y="Eixo Y")
p8

x_depois<-c(0.02,0.86,0.06,0.13,0.15,0.2,0.2,0.2,0.24,0.28,0.3,0.335,0.38,0.38,0.43,0.47,0.49,0.52,0.56,0.59,0.61,0.63,0.09,0.67,0.69,0.72,0.76,0.76,0.81,0.9)
y_depois<-c(0.08,0.93,0.11,0.16,0.45,0.21,0.38,0.47,0.58,0.6,0.24,0.26,0.31,0.42,0.52,0.49,0.13,0.61,0.7,0.67,0.8,0.35,0.23,0.62,0.87,0.75,0.67,0.83,0.78,0.9)
p9 <- ggplot() +
  geom_point(aes(x = x_depois, y = y_depois), color = "blue", shape = 16) +
  labs(x="Eixo X",y="Eixo Y")
grid.arrange(p8,p9,nrow=1)

VT<-c(0.5,15,13,27,0.45,0.038,0.5,2,0.37,12.34,0.3,0.35,4.25,55,7,0.3,74,15,0.043,7.04,0.48,0.12,5.25,0.3,1.52,0.074,0.29,2.5,0.7)
VF<-c(0.17,1.8,3,3.5,0.07,0.011,0.085,0.5,0.37,9,0.038,0.35,1.9,32,0.085,0.09,23.6,6.8,0.021,0.6,0.08,0.025,4.2,0.2,0.22,0.038,0.19,2,0.28)
barragem1<-ggplot()+
  geom_point(aes(x=VT,y=VF),color="blue",shape=16)+
  labs(x="Eixo X",y="Eixo Y")
barragem2<-ggplot() +
  geom_point(aes(x=pobs(VT),y=pobs(VF)),color="blue",shape=16)+
  labs(x="Eixo X",y="Eixo Y")
grid.arrange(barragem1,barragem2,nrow=1)




















