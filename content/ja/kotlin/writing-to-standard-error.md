---
title:                "「標準エラーへの書き込み」"
html_title:           "Kotlin: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする際、標準エラー（standard error）に書き込むことは非常に重要です。エラーが発生した際に、標準エラーに書き込むことで、エラーの原因やその詳細を把握することができます。これにより、バグを見つける際に役立ちます。

## 方法

ここでは、Kotlinを使用して標準エラーに書き込む方法を紹介します。まず、次のように`System.err`を使用して標準エラーに書き込みます。

```Kotlin
System.err.println("エラーメッセージ");
```

上記のコードを実行すると、コンソールにエラーメッセージが表示されます。また、`System.err`の代わりに`System.out`を使用することで、エラーではなく通常のメッセージを標準出力（standard output）に書き込むこともできます。

## 深堀り

標準エラーに書き込むことで、エラーの詳細を把握できます。例えば、以下のように例外（exception）のスタックトレース（stack trace）を標準エラーに書き込むことができます。

```Kotlin
try {
  // 例外が発生する可能性のあるコード
} catch (e: Exception) {
  // 例外が発生した場合、スタックトレースを標準エラーに書き込む
  e.printStackTrace(System.err)
}
```

このようにすることで、エラーが発生した箇所を特定し、バグを見つけることができます。

## 参考リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/reference/)
- [JavaプログラマーのためのKotlin入門](https://www.tohoho-web.com/ex/kotlin.html)
- [Kotlinで例外処理を作成する方法](https://www.afternerd.com/blog/kotlin-exceptions/)