---
title:    "Go: 文字列の連結"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結に取り組むのかという理由を簡単に説明します。

Go言語では、文字列を1つの変数にまとめることができない場合があります。そのような場合、複数の変数を連結して1つの文字列として扱うことで、より柔軟なコードを書くことができます。

## 方法

以下に、Go言語で文字列を連結する方法をコーディング例とともに紹介します。

```Go
package main

import "fmt"

func main() {
  // 文字列を連結する方法
  str1 := "Hello "
  str2 := "World!"
  result := str1 + str2
  fmt.Println(result) // 出力: Hello World!
}
```

上記のコードでは、`+`演算子を使用して2つの文字列を連結しています。最終的な結果は `"Hello World!"` となります。

## ディープダイブ

初心者の方にとっては、文字列の連結は非常に基本的な操作かもしれません。しかし、実はGo言語にはより高度な連結方法が存在します。

例えば、`fmt.Sprintf()`関数を使用して任意のフォーマットに基づいて文字列を連結することができます。また、`bytes.Buffer`パッケージを使用することで、より高速な連結処理を実現することもできます。

詳細な情報を知りたい方は、[公式ドキュメント](https://golang.org/pkg/strings/#Concat)を参照してください。

## 参考リンク

- [Go公式ドキュメント - strings#Concat](https://golang.org/pkg/strings/#Concat)
- [組み込み関数 - string concatenation](https://golang.org/ref/spec#String_concatenation)
- [Go Lang Cheat Sheet - String operations](https://github.com/a8m/go-lang-cheat-sheet#string-operations)

## 関連リンク

- [Go言語チュートリアル - 文字列の操作](https://www.tohoho-web.com/ex/golang.html#link15)
- [はじめてのGo言語 - 文字列の操作](https://qiita.com/tenntenn/items/0e33a495925f63c2d45b)
- [Go言語で文字列を連結する方法](https://qiita.com/ogady/items/e776ce1c924909cea5af)