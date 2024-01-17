---
title:                "日付を文字列に変換する"
html_title:           "Fish Shell: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

日付を文字列に変換することは、プログラマーが日付をコンピュータで取り扱うための方法です。日付を文字列に変換することで、表示や処理が容易になります。

## How to:

```Fish Shell``` のコードブロック内には、日付を文字列に変換するためのコーディング例と出力結果があります。

```
set -l today (date +%m/%d/%Y)

echo $today

Output: 05/24/2021
```

```Fish Shell``` では、```date``` コマンドを使用して日付情報を取得し、```+%m/%d/%Y``` の形式で日付を文字列に変換します。変数 ```today``` に代入された日付は、```echo``` コマンドを使用して表示されます。

## Deep Dive

日付を文字列に変換する方法は、コンピュータの利用が一般化する前から存在し、ほとんどのプログラミング言語で標準的な機能として提供されています。代表的な形式としては、年・月・日の順に並べるYYYYMMDDや、月・日・年の順に並べるMMDDYYYYがあります。また、日付を文字列に限らず、数値やISOフォーマットにも変換することができます。

日付を文字列に変換する方法として、他にもグリニッジ標準時や協定世界時などの国際的な規格が存在します。これらの規格は、異なるタイムゾーンや夏時間の取り扱いなど、日付を正確に表すためのものです。プログラマーは、それぞれの規格に応じて適切な形式で日付を文字列に変換する必要があります。

日付を文字列に変換する方法は様々ありますが、```Fish Shell```ではデフォルトで```date``` コマンドを使用することで簡単に実装することができます。また、PythonやJavaなどのプログラミング言語でも同様の変換を行うことができ、さらに多様なオプションを提供しています。

## See Also

- [Fish Shell Documentations](https://fishshell.com/docs/current/index.html)
- [ISO 8601 Standard for Date and Time Representations](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Python Date Formatting](https://docs.python.org/3/library/datetime.html)