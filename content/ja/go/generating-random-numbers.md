---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

ランダムナンバー生成は、望む範囲内で予測不能な数字を作出するプロセスです。これはテストデータを作ったり、ゲームプログラミングで非決定性を導入したりするためにプログラマーによく利用されます。

## 実装方法:

以下のコードは Go でランダムな整数を生成する方法の一つです。

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())
    randomNum := rand.Intn(100) // 0から99までのランダムな数
    fmt.Println(randomNum)
}
```

このプログラムを実行すると、0 から 99 までのランダムな数字が出力されます。例: 42, 69, 23 など。

## ディープダイブ:

ランダム数値生成の歴史は古く、それらのアルゴリズムはコンピュータの初期段階から使われてます。Goでの乱数生成には主に "math/rand" パッケージと "crypto/rand" パッケージが利用されますが、後者はより高度なセキュリティが求められる場合に使用されます。

また、Goでは乱数生成器のシード値を設定することが重要であり、これが決定的なランダム値を作成します。上記の例では `time.Now().UnixNano()`をシードとして使用しています。

## 参考資料:

以下のリンクは、Goのランダム数値生成に関連する更なる情報を提供します。

1. Go公式ドキュメンテーション: [rand package - math/rand](https://golang.org/pkg/math/rand/)
2. Go公式ドキュメンテーション: [rand package - crypto/rand](https://golang.org/pkg/crypto/rand/)
3. The Go Playground: [Random number generator example](https://play.golang.org/p/_Z4koiVmrFQ)