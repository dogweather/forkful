---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:08.119414-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.041160-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングにおいて文字列の先頭文字を大文字に変換することは、それがまだそうでない場合に有用であり、ユーザー入力のフォーマットや、より標準化された、またはユーザーフレンドリーな方法でユーザーインターフェースにテキストを表示するのに役立ちます。プログラマーは、ソフトウェアアプリケーション内の特定のフォーマット要件を満たすため、またはデータの一貫性を保証するために、この操作を行います。

## 方法：

Kotlinでは、標準ライブラリ関数を使用して文字列を大文字にすることができ、サードパーティのライブラリは必要ありません。Kotlinの文字列処理方法により、これらの操作は簡潔でわかりやすくなっています。

### 文字列全体を大文字化する：

```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // 出力: HELLO, WORLD!
```

### 最初の文字のみを大文字化する：

Kotlin 1.5以降、`capitalize()`関数は非推奨とされ、`replaceFirstChar`と、それが小文字であれば大文字に変換するラムダを組み合わせて置き換えられました。

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // 出力: Hello, world!
```

この方法では、文の残りの部分をそのまま維持しつつ、最初の文字のみを大文字に変更します。
