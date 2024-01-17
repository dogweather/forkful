---
title:                "ランダムな数字を生成する"
html_title:           "Go: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

アーティクル

Goでランダムな数字を生成する方法

## 何 & 何のため？

ランダムな数字を生成するとは、プログラマーがコード内でランダムな値を作成することです。ランダムな数字を使用する理由はさまざまですが、ゲームや暗号化、テストデータの生成などによく使われます。

## 方法：

Go言語では、math/randパッケージを使用してランダムな数字を生成することができます。下記のコード例を参考にしてください。

```Go
package main
import (
  "fmt"
  "math/rand"
  "time"
)
func main() {
  // 現在の時刻をシード値として設定する
  rand.Seed(time.Now().UnixNano())
  
  // 0から10の範囲でランダムな整数を生成する 
  fmt.Println(rand.Intn(11))
  
  // float64型のランダムな数字を生成する
  fmt.Println(rand.Float64())
}
```

上記のコードを実行すると、それぞれの実行ごとに異なるランダムな数字が出力されることがわかります。

## 詳細情報：

ランダムな数字を生成する方法にはいくつかの方法がありますが、Go言語では標準でmath/randパッケージが提供されています。他の言語では、乱数生成器や暗号グラフを使用するなどの方法があります。

ランダムな数字の生成には、それぞれに異なるアルゴリズムが使用されますが、一般的には擬似乱数が使用されます。擬似乱数は、予測不能でありながらも再現可能な数字のことです。

## 関連情報：

Go言語のmath/randパッケージのドキュメント: https://golang.org/pkg/math/rand/

擬似乱数の詳細な解説: https://en.wikipedia.org/wiki/Pseudorandom_number_generator