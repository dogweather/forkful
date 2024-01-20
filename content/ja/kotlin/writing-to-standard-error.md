---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
標準エラーに書き込むとは、エラーメッセージや警告を標準出力とは分けて出力することです。プログラムの実行中に問題が発生した際、メッセージをユーザーや別のプログラムに効率的に通知するために使われます。

## How to: (方法)
### 標準エラーへの書き込み:
```Kotlin
fun main() {
    System.err.println("Error: ファイルを読み込めませんでした。")
}
```
### 出力サンプル:
```
Error: ファイルを読み込めませんでした。
```

## Deep Dive (深堀り)
標準エラーへの書き込みはUNIXの時代からあります。`System.err`はJavaから継承されてKotlinでも利用できる。標準出力`System.out`と分けて使うことで、エラーメッセージを別のファイルやツールにリダイレクトしやすくなります。`printStackTrace()`などの例外処理メソッドも内部的には`System.err`を使用しています。

## See Also (関連情報)
- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/home.html)
- [The Java™ Tutorials - I/O Streams](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [UNIXの歴史について](https://ja.wikipedia.org/wiki/UNIX)