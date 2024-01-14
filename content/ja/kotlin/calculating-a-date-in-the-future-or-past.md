---
title:    "Kotlin: 「未来または過去の日付を計算する」"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ

日付を計算することは、将来や過去の特定の日付を見つけるために非常に便利です。プロジェクトの納期を計画したり、締め切りを守るために必要な日付を把握したりするのに役立ちます。今回は、Kotlinを使って日付の計算をする方法をご紹介します。

## 使い方

日付を計算するには、まずはじめに`LocalDate`クラスを使います。これは、日付を表すためのクラスです。今回は、簡単な例として、今日の日付から5日後の日付を計算してみましょう。

```Kotlin
// LocalDateオブジェクトを作成
val today = LocalDate.now()

// 5日後の日付を計算
val futureDate = today.plusDays(5)

// 結果を出力
println("5日後の日付は${futureDate}です。")

```

実行すると、以下のように出力されます。

```Kotlin
5日後の日付は2021-10-11です。
```

また、過去の日付を計算する場合は、`minus`メソッドを使います。例えば、10日前の日付を計算するコードは以下のようになります。

```Kotlin
// LocalDateオブジェクトを作成
val today = LocalDate.now()

// 10日前の日付を計算
val pastDate = today.minusDays(10)

// 結果を出力
println("10日前の日付は${pastDate}です。")
```

実行すると、以下のように出力されます。

```Kotlin
10日前の日付は2021-09-26です。
```

このように、`plus`メソッドと`minus`メソッドを使うことで、簡単に日付の計算ができます。

## 深堀り

日付の計算についてさらに詳しく知りたい方は、`LocalDate`クラスのドキュメントを参照してください。また、`plus`メソッドや`minus`メソッド以外にも、`LocalDate`クラスにはさまざまなメソッドがありますので、ぜひ調べてみてください。

それでは、日付の計算を楽しんでください！

## 関連リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/home.html)
- [Java LocalDateクラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDate.html)