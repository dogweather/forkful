---
title:                "文字列の補間"
html_title:           "Go: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何 & なんで？
文字列の内挿とは、文字列の中に変数や式を埋め込むことを指します。プログラマーがこれを行うのは、プログラムをより柔軟にし、より簡潔に書くためです。

## 方法：
文字列の内挿は、Go言語で非常に簡単に行うことができます。 ```Go fmt.Printf() function ```や ```Go fmt.Sprintf() function ```を使って、変数や式を文字列に埋め込むことができます。例を見てみましょう：

```Go
name := "John"
age := 25
fmt.Printf("私の名前は%sで、%d歳です。", name, age)
```

出力結果は以下のようになります：

```
私の名前はJohnで、25歳です。
```

## 深堀り：
文字列の内挿は、かつては ```Go string.Format() ```を使用して行われていましたが、現在は ```Go fmt.Printf() ```や ```Go fmt.Sprintf() ```が主流となっています。他のプログラミング言語では、 ```Go printf() ```や ```Go sprintf() ```が同様の機能を持っています。

## 参考：
- [Go fmt パッケージドキュメンテーション](https://golang.org/pkg/fmt/)
- [文字列の内挿についてのブログ記事](https://blog.golang.org/slices)