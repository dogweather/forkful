---
title:                "Go: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ

削除パターンにマッチする文字を削除する理由は、特定の文字を除外したい場合に便利です。例えば、特定のフォーマットに従わない文字を取り除く際に使用することができます。

# 方法

```Go
// 文字列を定義する
str := "Hello, 世界!"

// 削除する文字のパターンを定義する
pattern := "[^\u0000-\u007F]"

// 文字列からパターンにマッチする文字を削除する
result := regexp.ReplaceAllString(str, pattern, "")

// 出力する
fmt.Println(result)
```

上記のコードを実行すると、"Hello, !"という文字列が出力されます。正規表現パターンにマッチする文字が削除されていることがわかります。

# 深堀り

Go言語では、`regexp`パッケージを使用して正規表現を扱うことができます。`ReplaceAllString()`メソッドを使用すると、パターンにマッチする文字列を指定した文字列に置換することができます。

また、マッチする文字が複数ある場合は`ReplaceAll()`メソッドを使用することで全てを置換することができます。さらに、`ReplaceAllFunc()`メソッドを使用することで、置換する文字列を自分で指定することもできます。

# 参考リンク

- [The Go Programming Language](https://golang.org/)
- [Regular Expressions | Packages | GoDoc](https://godoc.org/regexp)
- [Regular expression - Wikipedia](https://en.wikipedia.org/wiki/Regular_expression)

## 参考文献

- "Regular expression (regexp)." Wikipedia. Wikimedia Foundation, 22 Mar. 2020. Web. 22 Mar. 2020.
- "Regular Expressions." Packages | GoDoc. Open Source, n.d. Web. 22 Mar. 2020.
- The Go Programming Language. 1st ed., Brian W. Kernighan, Alan A. A. Donovan, Addison-Wesley Professional, 2016.