---
title:    "Kotlin: 正規表現を使う"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか？

正規表現は、文字列のパターンを検索や置換するための強力なツールです。例えば、入力されたパスワードが指定された要件を満たしているかどうかをチェックしたり、テキストデータから必要な情報を抜き出したりするのに便利です。

## 正規表現の使い方

```Kotlin
val emailPattern = "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9-.]+$".toRegex() // パターンを定義

val email1 = "test@example.com"
val email2 = "invalid_email" // 不正な形式

println(email1.matches(emailPattern)) // 結果: true
println(email2.matches(emailPattern)) // 結果: false
```

上記のコードでは、メールアドレスの正しい形式をチェックするための正規表現を定義し、それを実際のメールアドレスに適用して結果を検証しています。

## 正規表現の詳細

正規表現では、様々な検索パターンや置換パターンを定義することができます。特殊文字やメタ文字を使うことで、より複雑なパターンを定義することができます。また、グループやバックリファレンスを使うことで、パターンにマッチした部分を取得したり、置換時に活用することができます。

正規表現は強力なツールですが、パターンを定義する際には注意が必要です。間違ったパターンを定義すると、予期せぬ結果を招く可能性があります。また、パターンを理解するためには学習が必要です。複雑なパターンを定義する際には、正規表現のチートシートやツールを活用することもおすすめです。

## 参考リンク

- [Kotlin チュートリアル：正規表現](https://kotlinlang.org/docs/tutorials/regular-expressions.html)
- [正規表現構文のチートシート](https://www.rexegg.com/regex-quickstart.html)
- [正規表現エディタ・テスター](https://regex101.com/)