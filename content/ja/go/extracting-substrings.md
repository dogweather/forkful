---
title:                "Go: サブストリングの抽出"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
サブストリングを抽出する理由は、より柔軟な文字列操作が可能になるためです。例えば、文字列内の特定の部分を取得したり、不要な部分を取り除いたりすることができます。

## 方法
サブストリングを抽出するには、Goの `strings` パッケージに含まれている `Substr` 関数を使用します。以下のようにコードを記述することで、文字列から特定の部分を取り出すことができます。

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Hello, World!"
    substr := strings.Substr(text, 7, 5)
    fmt.Println(substr)
}
```

上記のコードを実行すると、出力結果は `World` となります。ここで、 `Substr` 関数は、抽出する文字列の開始位置と抽出する文字数を引数として受け取ることができます。

## 深堀り
サブストリングを抽出する際には、以下のようなことに気をつける必要があります。

- 開始位置と抽出する文字数は、必ず正の値で指定する必要があります。負の値を指定するとエラーが発生します。
- 開始位置は、抽出する文字列の先頭を 0 としたインデックスとして指定する必要があります。つまり、 `Hello, World!` の場合、 `H` は 0、`W` は 7 となります。

また、 `Substr` 関数は `strings` パッケージの中で定義されているため、必ず `import "strings"` を記述する必要があります。さらに、 `strings` パッケージには他にも便利な関数が多数含まれているので、ぜひチェックしてみてください。

## 関連リンク
- [strings パッケージドキュメント](https://golang.org/pkg/strings/)
- [Learn Go with Tests - strings](https://github.com/quii/learn-go-with-tests/tree/master/strings)
- [Welcome to the Golang Tour - strings](https://tour.golang.org/basics/5)