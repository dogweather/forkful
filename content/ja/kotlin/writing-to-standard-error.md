---
title:                "Kotlin: 「標準エラーに書き込むこと」"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

エラーを書き込むことには、いくつかのメリットがあります。最も一般的なのは、プログラムの実行時に発生したエラーをキャッチするためです。また、デバッグやトラブルシューティングの際にもエラーの詳細が必要になる場合があります。そういったケースでは、エラーを標準エラー出力に書き込むことで、問題の特定や解決に役立ちます。

## 使い方

標準エラー出力に書き込むには、 `System.err.println()` メソッドを使用します。以下のように、エラーメッセージを文字列として渡すことで、エラーを書き込むことができます。

```Kotlin
// エラーメッセージを書き込む
System.err.println("Error occurred.")

// 変数の値を含むエラーメッセージを書き込む
val num = 5
System.err.println("Error: The value is $num")
```

上記のコードを実行すると、次のような出力が得られます。

```
Error occurred.
Error: The value is 5
```

## 詳細を見る

標準エラー出力を使用すると、エラーの詳細をプログラム実行中に確認することができます。また、これらの詳細をログファイルに書き込むことで、後から調査やトレースが可能になります。さらに、標準エラー出力を使用することで、エラーが発生した箇所や原因を特定しやすくなります。

## さらに見る

* [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/index.html)
* [Using the Standard Error Output Stream in Java](https://www.baeldung.com/java-write-to-standard-error)
* [Debugging Kotlin Code with Logback](https://www.baeldung.com/kotlin-logback)
* [Kotlin Logging Tutorial](https://www.codevscolor.com/kotlin-logging/)