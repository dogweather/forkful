---
title:                "Kotlin: 「将来または過去の日付を計算する」"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を計算することの理由は多様です。新しいアプリケーションを開発する際や、既存のアプリケーションに機能を追加する際に、現在の日付から特定の日数を加えたり引いたりする必要があるかもしれません。また、将来のイベントの日程を計算するためにも日付の計算が必要です。

## 使い方

Kotlinでは、日付を計算するためのさまざまな方法が用意されています。例を挙げると、```LocalDate```クラスを使用して現在の日付を取得し、```plusDays()```メソッドを使って指定した日数を加えることができます。そして、```format()```メソッドを使用して、計算された日付を特定のフォーマットに変換することができます。

```
Kotlin
fun main() {
    val today = LocalDate.now()
    val futureDate = today.plusDays(30)
    println(futureDate.format(DateTimeFormatter.ofPattern("yyyy/MM/dd")))
}
```

上記のコードを実行すると、現在の日付から30日後の日付をyyyy/MM/ddのフォーマットで表示することができます。

## ディープダイブ

日付を計算する際には、日付と時刻を表すさまざまなクラスが利用できます。```LocalDate```クラスだけではなく、例えば```LocalDateTime```クラスを使用することで、日付と時刻を同時に計算することも可能です。また、今回紹介したメソッド以外にも、日付の計算に役立つたくさんのメソッドが提供されていますので、ぜひ公式ドキュメントを参照してみてください。

## 関連リンク

[Kotlin公式ドキュメント](https://kotlinlang.org/docs/home.html)
[Kotlin日付と時刻の操作](https://kotlinlang.org/docs/dates-and-times.html)