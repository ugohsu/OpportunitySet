###
###     関数の定義
###

## 相関行列を共分散行列に変換

cor2cov <- function(x) {
    rslt <- x                           # アウトプット用の変数
    for (i in 1:nrow(x)) {
        for (j in 1:ncol(x)) {
            if (i == j) {               # 対角成分
                rslt[i, j] <- x[i, j] ^ 2
            } else {                    # 共分散
                rslt[i, j] <- x[i, j] * x[i, i] * x[j, j]
            }
        }
    }
    return (rslt)
}


## ウェイトを組み合わせて 3 つの証券のウェイトを作成

mkWeight <- function(length) {
    x <- seq(0, 1, length=length)
    rslt <- data.frame(
        A = x,
        B = 1 - x
    )
    return (rslt)
}


## ウェイト、リターン、リスクからデータを作成

mkData <- function(w, R, V) {
    MyMeans <- numeric(nrow(w))
    MyStd <- numeric(nrow(w))

    for (i in 1:nrow(w)) {
        W <- t(as.vector(w[i,]))
        MyMeans[i] <- t(W) %*% as.vector(R)
        MyStd[i] <- (t(W) %*% V %*% W) ^ 0.5
    }

    return (data.frame(
        Risk = MyStd,
        Return = MyMeans
    ))
}


###
###     変数の定義
###

## リターンと相関行列の定義 (相関で考えたほうがわかりやすいので)

R <- c(0.02, 0.03, 0.06)                # リターン
C <- rbind(                             # 相関 (対角成分は標準偏差)
    c(0.05, -0.02, -0.52),
    c(-0.02, 0.10, 0.45),
    c(-0.52, 0.45, 0.14)
)

## 共分散行列の作成

V <- cor2cov(C)


###
###     ウェイトの作成
###

## まずは、A と B のウェイト作成

wdot <- data.frame(mkWeight(11), C=numeric(11)) # ドットで作図するときに必要
wline <- data.frame(mkWeight(301), C=numeric(301)) # 線で作図するときに必要
wC <- mkWeight(101)                                # AB ポートフォリオと証券 C の組み合わせ

## wline (AB ポートフォリオ) と証券 C の組み合わせについて、
## wline のウェイトの組み合わせ数 (301 通り) の長さのリストを
## 作成し、各パターンと、証券 C とのポートフォリオのリストを作成する
weightList <- as.list(NULL)

for (i in 1:nrow(wline)) {
    AB <- wline[i,]
    weightList[[i]] <- data.frame(
        A = wline[i, 1] * wC$B,
        B = wline[i, 2] * wC$B,
        C = wC$A
    )
}


###
###     グラフの作成
###

png("./fig/fig01.png")
plot(mkData(wdot, R, V), col="blue", xlim=c(0, 0.14), ylim=c(0, 0.06))
dev.off()

png("./fig/fig02.png")
plot(mkData(wline, R, V), col="blue", xlim=c(0, 0.14), ylim=c(0, 0.06), type="l")
dev.off()

for (n in 1:length(weightList)) {
    png(paste("./fig/fig03_", sprintf("%04d", n), ".png", sep=""))
    plot(mkData(wline, R, V), col="blue", xlim=c(0, 0.14), ylim=c(0, 0.06), type="l")
    for (i in 1:n) {
        par(new = T)
        plot(
            mkData(weightList[[i]], R, V),
            col="blue", xlim=c(0, 0.14), ylim=c(0, 0.06), type="l"
        )
    }
    dev.off()
}
