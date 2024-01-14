---
title:    "Go: ランダムな数を生成する"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
Go言語でランダムな数字を生成するのに気分的な用途は、プログラミングの実践の一部であることが多いです。たとえば、ランダムなコードを生成して、ゲームやシミュレーションを作成することができます。

## 方法
```Go
package main

// math/randパッケージをインポートします
import (
    "fmt"
    "math/rand"
)

func main() {
    // Intn関数を使用して、0から9までのランダムな整数を生成します
    fmt.Println("ランダムな数字: ", rand.Intn(10))
    
    // Seed関数を使用して、同じシード値からのランダムシーケンスを生成します
    rand.Seed(42)
    
    // Intn関数を再度使用して、0から9までのランダムな整数を生成します
    fmt.Println("同じシード値からのランダムな数字: ", rand.Intn(10))
}
```
出力:
```
ランダムな数字:  7
同じシード値からのランダムな数字:  7
```

## ピッチダイブ
Go言語では、math/randパッケージを使用してランダムな数字を生成することができます。Go言語では、デフォルトでは同じシード値を使用してIntn関数を呼び出すたびに同じランダムシーケンスが生成されるため、Seed関数を使用して異なるシード値を設定することが重要です。

## 参考
- [A Tour of Go: Random Numbers](https://tour.golang.org/basics/23)
- [math/randパッケージドキュメント](https://golang.org/pkg/math/rand/)
- [Go言語チュートリアル: ランダム数値](https://www.golangtutorial.dev/tips/random-number/)