---
title:                "文字列を小文字に変換する"
html_title:           "Go: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することに関心がある理由は、大文字と小文字を区別せずに比較や検索を行いたい場合があるからです。

## 使い方

```Go
strings.ToLower("Hello") // "hello"
strings.ToLower("GOTHAM") // "gotham"
```

## ディープダイブ

文字列を小文字に変換するには、Go言語の組み込み関数である`strings.ToLower()`を使用します。このメソッドは、引数として与えられた文字列をすべて小文字に変換した新しい文字列を返します。また、大文字のアルファベット以外の文字列は変換せずにそのまま返されます。

## See Also
- [Go言語のstringsパッケージ](https://golang.org/pkg/strings)
- [小文字の変換の例外処理](https://tour.golang.org/flowcontrol/13)