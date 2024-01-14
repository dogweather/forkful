---
title:                "Kotlin: 正規表現の使用方法"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜRegular Expressionsを使うのか

Regular Expressions（正規表現）は、文字列のパターンを検索したり、置換したりするための強力なツールです。例えば、メールアドレスや電話番号など、特定のフォーマットに従う文字列を抽出したい場合や、大量のテキストデータから特定のキーワードを見つけたい場合に便利です。

## 使い方

### パターンの検索

以下のように`Regex()`を使い、検索したい文字列のパターンを指定することで、正規表現を作成することができます。
```Kotlin
val regex = Regex("pattern")
```
作成した正規表現を使い、文字列から特定のパターンを検索することができます。例えば、以下のようにメールアドレスを検索することができます。
```Kotlin
val emailRegex = Regex("[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+")
val text = "私のメールアドレスはexample@example.comです。"
val matchResult = emailRegex.find(text)
println(matchResult?.value) // output: example@example.com
```

### パターンの置換

すでに作成した正規表現を使い、文字列内の特定のパターンを置換することもできます。例えば、以下のように電話番号を置換することができます。
```Kotlin
val phoneNumberRegex = Regex("\\d{3}-\\d{4}-\\d{4}")
val text = "私の電話番号は123-4567-8901です。"
val replacedText = phoneNumberRegex.replace(text, "000-0000-0000")
println(replacedText) // output: 私の電話番号は000-0000-0000です。
```

## 深堀り

### 特殊文字

正規表現の中には、特殊な意味を持つ文字があります。例えば、`.`はどんな文字にもマッチするワイルドカードとして使われます。また、`+`は直前の文字が1回以上繰り返されることを表します。これらの特殊文字を使いこなすことで、より複雑なパターンを検索したり置換したりすることができます。

### バックスラッシュ

正規表現の中には`\`を使うことができますが、Kotlinではエスケープシーケンスとして扱われます。そのため、バックスラッシュを使う場合は、2つ並べる必要があります。例えば、`\d`は数字にマッチするという意味ですが、そのまま文字列として表現する場合は`\\d`となります。

### n個の文字にマッチ

正規表現の中には、特定の文字がn個繰り返されることを表すことができます。例えば、`{n}`を使い、数字が3回繰り返されることを表すには、`\\d{3}`という表現を使います。

## おすすめリンク

### [Kotlin公式ドキュメント - Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text.-regex/index.html)
KotlinのRegexクラスの公式ドキュメントです。より詳細な使い方やメソッドの説明を確認できます。

### [正規表現チュ