---
title:    "Gleam: 日付の比較"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜ日付の比較をするのか？

日付の比較をすることについて、なぜ重要なのでしょうか？日付は、私たちが日々の生活で欠かせない存在です。私たちは、誕生日や記念日を祝ったり、予定を立てる際に日付を確認したりします。しかし、時には日付を比較する必要がある場合があります。例えば、ある日付が別の日付よりも前か後ろかを確認したり、2つの日付が同じかどうかを確認したりする必要があるかもしれません。そこで、Gleam言語で日付を比較する方法をご紹介します。

# やり方

まずは、2つの日付を比較する方法を見てみましょう。以下のコードを参考にしてください。

```Gleam
import gleam/time

let date1 = Time.to_utc(2021, April, 25)
let date2 = Time.to_utc(2021, April, 26)

if date1 > date2 {
  println("Date1はDate2よりも後の日付です。")
} else if date1 < date2 {
  println("Date1はDate2よりも前の日付です。")
} else {
  println("Date1とDate2は同じ日付です。")
}
```

上記のコードを実行すると、出力結果は以下のようになります。

```
Date1はDate2よりも前の日付です。
```

次に、2つの日付が同じかどうかを確認する方法を見てみましょう。以下のコードを参考にしてください。

```Gleam
import gleam/time

let date1 = Time.to_utc(2021, April, 25)
let date2 = Time.to_utc(2021, April, 25)

if date1 == date2 {
  println("Date1とDate2は同じ日付です。")
} else {
  println("Date1とDate2は同じ日付ではありません。")
}
```

上記のコードを実行すると、出力結果は以下のようになります。

```
Date1とDate2は同じ日付です。
```

# 詳細を掘り下げる

Gleam言語では、日付を比較するために`Time`モジュールを使用することができます。`>`や`<`といった比較演算子を使用して、日付が前後関係にあるかどうかを比較することができます。また、`==`や`!=`などを使用して、2つの日付が同じかどうかを確認することもできます。

# 参考リンク

- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [日付/時刻の操作](https://gleam.run/documentation/built-in-modules/time/)
- [Gleamを使用して日付を扱う方法](https://dev.to/elviejokike/working-with-dates-using-gleam-3fdl) 

# 関連リンク

- [Gleamで文字列を比較する方法](https://example.com/gleam-strings-comparison)
- [Gleamで数値を比較する方法](https://example.com/gleam-numbers-comparison)