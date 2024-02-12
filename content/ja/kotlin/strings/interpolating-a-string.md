---
title:                "文字列の補間"
aliases:
- /ja/kotlin/interpolating-a-string.md
date:                  2024-01-20T17:51:31.244989-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは、文字列中に変数や表現を埋め込むことです。コードを簡潔に書くし、動的なコンテンツを生成するために使います。

## How to: (方法)
```kotlin
fun main() {
    val name = "山田"
    val age = 30
    // 文字列補間の使用例
    val greeting = "こんにちは、$nameさん。あなたは${age}歳ですね。"
    
    println(greeting)  // 出力: こんにちは、山田さん。あなたは30歳ですね。
}
```

## Deep Dive (深掘り)
Kotlinでは、文字列リテラル中に直接変数を埋め込めます。これが文字列補間と呼ばれる機能です。バッククォート(`$`)を使用し、複雑な式を使う場合はカーリーブラケット(`{}`)で囲みます。

歴史的には、文字列補間は多くのプログラミング言語で採用されており、各言語ごとに異なる構文を持っています。Kotlinの文字列補間は、他言語と比べて変数や式がシームレスに埋め込まれます。

代替手段として、古い言語では文字列の連結（`+` オペレーター）やフォーマット関数（`String.format()`など）を使って似た結果を得ることができますが、Kotlinの文字列補間はより簡潔で読みやすいコードを実現します。

内部実装として、コンパイラは文字列補間されたコードを文字列連結やビルダーを使って書き換えることで実行時のパフォーマンスも最適化しています。

## See Also (関連情報)
- Kotlinの公式ドキュメント: [Basic Types](https://kotlinlang.org/docs/basic-types.html#string-templates)
- 文字列補間に関するブログ記事: [Kotlin Tips: String Interpolation](https://blog.kotlin-academy.com/kotlin-tips-string-interpolation-3ead646d4fd8)
- 文字列操作の詳細: [Kotlin String manipulation](https://kotlinlang.org/docs/collections-overview.html)
