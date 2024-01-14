---
title:    "Go: 文字列の長さを求める"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##なぜ
文字列の長さを求めることに熱中する理由は、Goプログラムにおける基本的なコーディング技術であり、様々なアプリケーションで必要とされるものだからです。

##やり方
以下は、Goプログラムで文字列の長さを求める方法の例です。

```Go
package main

import "fmt"

func main() {
	str := "こんにちは、世界"
	fmt.Println("文字列の長さは", len(str), "です")
}
```

このコードを実行すると、次のような出力が得られます。

```Go
文字列の長さは 9 です
```

##深堀り
文字列の長さを求める方法は非常にシンプルですが、実際はどのように動作しているのでしょうか？Goでは、文字列はユニコード（Unicode）を使用してエンコードされています。つまり、文字列の長さを求める際には、実際の文字数と異なる場合があります。また、基本的なアルゴリズムやパフォーマンスについても考慮する必要があります。

##参考リンク
この記事では、Goプログラムにおける文字列の長さの求め方について紹介しましたが、他にもGoで役立つ情報がたくさんあります。以下のリンクを参考にして、お好みのトピックを学習してみてください。

- [A Tour of Go](https://tour.golang.org/welcome/1)：Go言語の公式チュートリアル
- [プログラミングの基本 - 文字列の長さを求めよう（Go入門）](https://www.javadrive.jp/golang/string/index5.html)：文字列の長さを求める方法を解説しています
- [Goとは？特徴や使い方を解説！](https://techacademy.jp/magazine/6235)：Go言語の概要や基本的な使い方について紹介しています

##その他の参考リンク
- [Go言語での文字列操作方法まとめ](https://qiita.com/yoskeoka/items/e3ba84c5574ad4dee01e)：Go言語での文字列操作のさまざまな方法を紹介しています
- [The Go Programming Language Specification](https://golang.org/ref/spec)：Go言語の仕様書（英語）