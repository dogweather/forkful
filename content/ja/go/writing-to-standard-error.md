---
title:                "Go: 標準エラーへの書き込み"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングにおいて、エラー処理は非常に重要です。コンソール画面に表示されるエラーメッセージは、開発者にとって非常に役に立ちます。様々な言語で使われているGo言語でも、エラーメッセージを出力する方法があります。今回は、標準エラー出力について紹介します。

## 方法

標準エラー出力は、`os.Stderr`を使用して出力することができます。例えば、以下のようにコードを記述することで、エラーメッセージを出力することができます。

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprint(os.Stderr, "エラーメッセージ")
}
```

上記のコードを実行すると、コンソール画面に「エラーメッセージ」という文字列が出力されます。

## 深堀り

Go言語では、標準出力と標準エラー出力が分けられています。標準出力は、一般的なメッセージを出力する際に使用します。一方、標準エラー出力は、エラーメッセージを出力する際に使用します。このように分けることで、エラーメッセージがすぐにわかるようになっています。

また、`fmt`パッケージの`Fprint`関数以外にも、`Fprintln`や`Fprintf`などの関数もあります。それぞれ、異なる出力方法が可能ですので、詳しく知りたい方は公式ドキュメントを参照してください。

## 詳しくはこちら

- [fmtパッケージドキュメント](https://golang.org/pkg/fmt/)
- [標準エラー出力について](https://gobyexample.com/stderr)
- [例外処理について](https://blog.golang.org/error-handling-and-go)

## 参考文献

［[Using os.Stderr for efficient logging](https://stackoverflow.com/a/29260008)］by [Dave Cheney](https://stackoverflow.com/users/6209/dave-cheney) - [Stack Overflow](https://stackoverflow.com/)に投稿された回答の一部を引用しました。