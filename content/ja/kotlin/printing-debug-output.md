---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何となぜ?

デバッグ出力の印刷は、コード中の値を検証する主要な手段です。プログラマーはデバッグ出力を使ってコードの実行中に何が起こるかを理解し、問題を解決しようとします。

## 手順:

Kotlinでは`println()`関数を使用してデバッグ出力を印刷します。例えば:

```kotlin
fun main() {
   var number = 5
   println("Number is: $number")
}
```

出力:

```
Number is: 5
```

`$`記号を使用して変数を文字列に組み込むことができます。

## ディープダイブ:

Kotlinの`println()`関数は、Javaの`System.out.println()`から直接派生しています。Javaで長い間標準的に使用されてきたコンソールへの出力方法です。

代わりの方法として、`print()`関数も利用できます。これは改行せずに値を表示します。また、ログラム（Log4j、SLF4Jなど）を使用してデバッグ出力を制御し、ファイルに出力したり、出力のレベルを設定したりすることも可能です。

具体的には、`println()`関数は実行時にコンソールに直接出力します。これは特にデバッグ目的で非常に便利ですが、パフォーマンスが必要な本番環境ではログツールを使用した方が良いかもしれません。

## 関連情報:

以下のリンクから更なる情報を得ることが可能です。

- Kotlinの公式ドキュメント: [Basic Types](https://kotlinlang.org/docs/basic-types.html#strings) 
- [Logging in Kotlin](https://www.baeldung.com/kotlin/logging)
- Duo Mobileのブログ: [5 Tips For Debugging in Kotlin](https://duo.com/decipher/five-tips-for-debugging-in-kotlin)