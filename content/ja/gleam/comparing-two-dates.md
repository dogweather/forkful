---
title:    "Gleam: 2つの日付を比較する"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

Gleam言語の日本の読者のためのカジュアルなブログ投稿

## Why

日付を比較することに興味がある方のために、Gleam言語で日付を比較する際の利点を紹介します。日付の比較は、日付の時間的関係を把握するために非常に便利です。例えば、イベントの予定を立てる際に、過去の日付と現在の日付を比較することで、どの日付がより近いかを判断することができます。

## How To

日付を比較するには、```Gleam.Date```モジュールを使用します。まず、比較したい二つの日付をそれぞれ```Gleam.Date.from_gregorian```関数で変換し、```Gleam.Date.compare```関数で比較します。次に、返された整数値を使用して、どちらの日付がより前か後かを判断できます。

以下は、このプロセスの例です。

```Gleam
import Gleam.Date

let today = Gleam.Date.from_gregorian(2021, 12, 15)
let yesterday = Gleam.Date.from_gregorian(2021, 12, 14)

let result = Gleam.Date.compare(today, yesterday)

// resultには1が格納されるので、今日の日付の方が後であることがわかります。
```

## Deep Dive

日付の比較は、単純に日付の前後関係を判断するだけでなく、より複雑な比較も可能です。例えば、月や年の違いを考慮できるように、```Gleam.Date.compare_with```関数を使用することもできます。

また、日付の比較には、異なるカレンダーを使用する場合も考慮する必要があります。Gleam言語では、グレゴリオ暦以外にもユリウス暦やイスラム暦などをサポートしているため、日付の比較を行う際には適切なカレンダーを指定することが重要です。

## See Also

Gleam言語に関する他の便利なモジュールは以下を参考にしてください。
- [Gleam.Date モジュールドキュメント](https://gleam.run/modules/gleam_stdlib/gleam_date)
- [Gleam 公式ドキュメント](https://gleam.run/)
- [Gleam 公式リポジトリ](https://github.com/gleam-lang/gleam)