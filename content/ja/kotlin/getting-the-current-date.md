---
title:                "「現在の日付を取得する」"
html_title:           "Kotlin: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

なぜ、現在の日付を取得しようとするのでしょうか。実は、私たちの日常生活やビジネスにおいて、日付はとても重要な情報です。例えば、スケジュール管理やデータベースのレコードに日付を追加したり、特定の日付をユーザーに表示したりするために、現在の日付を取得する必要があります。

## 方法

Kotlinを使用して、現在の日付を簡単に取得する方法を紹介します。まずは必要なインポートを行います。

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter
```

次に、現在の日付を取得するためのコードを書きます。

```Kotlin
val currentDate = LocalDate.now()
```

そして、取得した日付を任意のフォーマットで表示するために、```DateTimeFormatter```を使用します。

```Kotlin
val formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd")
val formattedCurrentDate = currentDate.format(formatter)
println("現在の日付は $formattedCurrentDate です。")
```

実行すると、以下のような結果が得られます。

```
現在の日付は 2021/09/10 です。
```

## 深堀り

今回使用した```LocalDate```クラスは、Java 8から導入された日付と時刻を扱うための新しいAPIです。このAPIを使用することで、より簡潔かつ柔軟な日付の取得や操作が可能になります。

また、今回は固定のフォーマットで日付を表示しましたが、```DateTimeFormatter```を使うことで任意のフォーマットに変更することができます。詳細なフォーマットの指定方法やパターンは、公式ドキュメントを参照することができます。

## 関連リンク

- [Kotlin 公式サイト](https://kotlinlang.org/)
- [Java 8 日付と時刻 APIの紹介](https://docs.oracle.com/javase/jp/8/docs/api/java/time/package-summary.html)
- [DateTimeFormatter クラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/format/DateTimeFormatter.html)