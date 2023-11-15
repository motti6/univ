library(pglm)
library(mfx)

data("UnionWage", package = "pglm")
df <- UnionWage

df$uvar <- as.integer(df$union=="yes")
df$black <- as.integer(df$com=="black")
df$hisp <- as.integer(df$com=="hispanic")
df$hvar <-  as.integer(df$health=="yes")
df$rvar <- as.integer(df$rural=="yes")
df$NE <- as.integer(df$region=="NorthEast")
df$S <- as.integer(df$region=="South")
df$NC <- as.integer(df$region=="NothernCentral")
df$y81 <- as.integer(df$year=="1981")
df$y82 <- as.integer(df$year=="1982")
df$y83 <- as.integer(df$year=="1983")
df$y84 <- as.integer(df$year=="1984")
df$y85 <- as.integer(df$year=="1985")
df$y86 <- as.integer(df$year=="1986")
df$y87 <- as.integer(df$year=="1987")

## Q1
model1 <- uvar ~ exper + school + married + black + hisp + hvar + rvar + NE +
  S + NC + y81 + y82 + y83 + y84 + y85 + y86 + y87

r1p <- glm(model1,data = df, family = binomial("probit"))

probitmfx(model1, atmean = FALSE, data = df)
probitmfx(model1, atmean = TRUE, data = df)

model12 <- uvar ~ exper + school + married + black + hisp + hvar + rvar + NE +
  S + NC + y81 + y82 + y83 + y84 + y85 + y86 + y87 + occ + sector

r12p <- glm(model12,data = df, family = binomial("probit"))

probitmfx(model12, atmean = FALSE, data = df)
probitmfx(model12, atmean = TRUE, data = df)

### 元のモデルに比べて、新たなダミー変数を投入したモデルでは、schoolを除いたほとんどの説明変数の限界効果が0方向に近づいている。

##Q2

base_model <- uvar ~ exper + school + married + black + hisp + hvar + rvar + NE +
  S + NC # + y81 + y82 + y83 + y84 + y85 + y86 + y87

dynamic_model <- uvar ~ lag(uvar) + exper + school + married + black + hisp + hvar + rvar + NE +
  S + NC # + y81 + y82 + y83 + y84 + y85 + y86 + y87

r210p <- pglm(base_model, data = df, family = binomial("probit"),
              start = c(rep(0,11), 0.1),model = "random", method = "bfgs", R = 7)
summary(r210p)

r220p <- pglm(dynamic_model, data = df, family = binomial("probit"),
              start = c(rep(0,12), 0.1),model = "random", method = "bfgs", R = 7)
summary(r220p)

### R=5
r211p <- pglm(base_model, data = df, family = binomial("probit"),
              start = c(rep(0,11), 0.1),model = "random", method = "bfgs", R = 5)
summary(r211p)

r221p <- pglm(dynamic_model, data = df, family = binomial("probit"),
              start = c(rep(0,12), 0.1),model = "random", method = "bfgs", R = 5)
summary(r221p)

### R=10
r212p <- pglm(base_model, data = df, family = binomial("probit"),
              start = c(rep(0,11), 0.1),model = "random", method = "bfgs", R = 10)
summary(r212p)

r222p <- pglm(dynamic_model, data = df, family = binomial("probit"),
              start = c(rep(0,12), 0.1),model = "random", method = "bfgs", R = 10)
summary(r222p)

### R=20
r213p <- pglm(base_model, data = df, family = binomial("probit"),
              start = c(rep(0,11), 0.1),model = "random", method = "bfgs", R = 20)
summary(r213p)

r223p <- pglm(dynamic_model, data = df, family = binomial("probit"),
              start = c(rep(0,12), 0.1),model = "random", method = "bfgs", R = 20)
summary(r223p)

### base_modelの方ではRの値を変更すると対数尤度も変わる一方で、dynamic_modelの方は比較的一貫性のある結果を得られた。