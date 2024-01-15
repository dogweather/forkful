---
title:                "パターンに一致する文字を削除する"
html_title:           "Go: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何故

Go言語の最新バージョンでは、文字列のパターンに一致する文字を削除することが可能です。この機能を利用することで、より効率的なコードを書くことができます。

## 方法

まずは、文字列から特定のパターンに一致する文字を削除する方法を見ていきましょう。下記のサンプルコードを参考にしてください。

```Go
package main

import "fmt"
import "strings"

func main() {
    // 元の文字列
    str := "Hello, こんにちは, 你好"
    // 削除したいパターンを指定
    pattern := "こんにちは"
    // strings.Replaceを使って、パターンと一致する文字を空文字に置き換える
    result := strings.Replace(str, pattern, "", -1)
    // 結果を出力
    fmt.Println(result)
}
```

上記のコードを実行すると、下記のような結果が表示されます。

```
Hello, , 你好
```

コード内のstrings.Replace関数では、第四引数に-1を指定しています。これにより、パターンと一致する文字をすべて削除することができます。また、第二引数で指定した文字列が複数回出現する場合でも、すべて削除されることに注意してください。

## 深堀り

Go言語で文字列のパターンに一致する文字を削除する方法を学びましたが、実はこの機能はどのように動いているのでしょうか？以下の点について、少しだけ深堀りしてみましょう。

- strings.Replace関数は、作成者であるRob Pike氏が発表した"正規表現は必要ない"という思想に基づいています。
- それにより、パフォーマンス面で優れています。
- また、Go言語のstringsパッケージは、文字列の操作に特化した高速なアルゴリズムを使用しています。そのため、意図しない結果が得られることなく、確実にパターンに一致する文字を削除することができます。

## 関連記事

他にもGo言語で役立つ記事をご紹介します。

- [Go言語で文字列操作を学ぼう](https://qiita.com/suin/items/98ba8f423e8445a50b41)
- [Goのstringsパッケージの使い方](https://www.yoheim.net/blog.php?q=20181001)
- [Rob Pike氏の"正規表現は不要論"についての考察](https://blog.jnito.com/entry/2018/03/18/202322)

## 参考リンク

- [Go言語の公式サイト](https://golang.org/)
- [Go by Example - strings.Replace](https://gobyexample.com/string-functions)
- [The Evolution of Go – Rob Pike](https://blog.golang.org/evolution)
- [The Go Programming Language Specification - Package strings](https://golang.org/ref/spec#Package_strings)