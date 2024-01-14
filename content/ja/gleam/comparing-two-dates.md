---
title:                "Gleam: 日付を比較する"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日々のプログラミング作業において、２つの日付を比較する必要があることはよくあります。例えば、ある作業が完了した日付と、その作業が開始された日付を比較することで、作業の進捗状況を把握することができます。Gleamでは、便利な関数を使って簡単に日付を比較することができます。

## 使い方

Gleamプログラミング言語では、日付を比較するために `DateTime` モジュールが用意されています。まずは、`DateTime` モジュールをインポートします。

```Gleam
import gleam/datetime
```

次に、比較したい２つの日付を定義します。

```Gleam
let start = DateTime.from_string("2021-01-01T00:00:00")
let end = DateTime.from_string("2021-01-15T12:00:00")
```

上記の例では、文字列から日付型への変換を行っていますが、他にも様々な方法で日付を定義することができます。詳細については、[DateTimeモジュールのドキュメント](https://gleam.run/modules/gleam_datetime.html)を参照してください。

そして、日付を比較するためには `DateTime.compare` 関数を使用します。この関数は、比較結果を整数値として返します。

```Gleam
DateTime.compare(start, end) // returns -1 (start is before end)
DateTime.compare(end, start) // returns 1 (end is after start)
DateTime.compare(start, start) // returns 0 (both are equal)
```

ここで返される整数値の意味は以下の通りです。

- `-1`: 最初の日付が２番目の日付よりも前にある
- `1`: 最初の日付が２番目の日付よりも後にある
- `0`: ２つの日付が等しい

このように、`DateTime.compare` 関数を使うことで、簡単に日付を比較することができます。

## ディープダイブ

Gleamの日付比較機能は、厳密なルールに従って実装されています。例えば、閏年やタイムゾーンの考慮などが行われています。また、使用できる演算子も `=`、`<`、`>`、`<=`、`>=` など幅広く揃っています。

Gleamの日付比較機能について詳しく知りたい場合は、[DateTimeモジュールのドキュメント](https://gleam.run/modules/gleam_datetime.html)を参照してください。

## 他の情報

- [Gleam公式サイト](https://gleam.run/)
- [Gleamの日付比較についてのチュートリアル](https://gleam.run/tour/overview)
- [DateTimeモジュールのドキュメント](https://gleam.run/modules/gleam_datetime.html)