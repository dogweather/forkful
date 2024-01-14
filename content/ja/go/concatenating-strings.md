---
title:                "Go: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

なぜGo言語で文字列を連結する必要があるのでしょうか？Go言語では、プログラムの実行速度が非常に重要です。そのため、文字列の連結方法も素早く効率的である必要があります。

## どのように

Go言語では、文字列を連結する方法はいくつかありますが、今回は `+` 演算子を使用する方法をご紹介します。以下のコード例では、`Hello`と`World`を連結し、`Hello World`という出力を得ることができます。

```Go
package main

import "fmt"

func main() {
  str1 := "Hello"
  str2 := "World"
  result := str1 + " " + str2
  fmt.Println(result)
}
```

出力：
```
Hello World
```

## 深堀り

文字列を連結する場合、1つの方法は `strings.Join()` 関数を使用することです。この方法では、複数の文字列を受け取り、指定した区切り文字を用いて連結することができます。以下のコード例では、`Hello`と`World`の間に`-`を挿入して連結しています。

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  strs := []string{"Hello", "World"}
  result := strings.Join(strs, "-")
  fmt.Println(result)
}
```

出力：
```
Hello-World
```

## 参考リンク

- [Go言語 公式ドキュメント](https://golang.org/)
- [Golangで文字列を連結する方法](https://qiita.com/tenntenn/items/10b5b41b1a2c2953d4ae)
- [Effective Go (日本語訳)](https://go-lang.xyz/effective.go.html#strings)

## 関連リンク

- [↑↑↑ 標準出力のフォーマット方法](https://example.com)
- [左右のスペースを削除する方法 →→→](https://example.com)