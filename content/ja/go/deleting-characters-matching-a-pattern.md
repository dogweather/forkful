---
title:                "Go: パターンに一致する文字を削除する"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ？

文字のパターンにマッチする文字を削除する理由は、特定のテキスト処理が必要な場合に役立ちます。例えば、ユーザーから入力されたデータを正規化するために使用することができます。

## 手順

これを行うためには、まずstringsパッケージのReplaceAll()関数を使用して、対象の文字列から特定のパターンを探し出し、それらを空の文字列に置き換えます。

```
import "strings"

text := "こんにちは、今日は良い天気です！"
pattern := "今日は"
replacement := ""

result := strings.ReplaceAll(text, pattern, replacement)

fmt.Println(result)
```
上記のコードを実行すると、"こんにちは、良い天気です！"という結果が得られます。

## 深堀り

ReplaceAll()関数は、指定されたパターンに完全に一致する文字列を検索し、そのパターンに一致する部分をすべて置き換えます。この関数にはオプションの引数もあり、大文字・小文字を区別しない検索や、置き換える回数を制限することもできます。

## 他に見るもの

- [strings.ReplaceAll() function documentation](https://golang.org/pkg/strings/#ReplaceAll)
- [Regular expressions in Go](https://golang.org/pkg/regexp/)