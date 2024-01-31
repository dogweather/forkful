---
title:                "将来または過去の日付を計算する"
date:                  2024-01-20T17:31:10.426870-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

category:             "Fish Shell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

将来または過去の日付を計算するとは、特定の日から日数を加えたり引いたりして、新しい日付を得ることです。プログラマーは、イベントのスケジューリング、期限の追跡、または過去のデータ分析のためにこれを行います。

## How to: (やり方)

Fish Shellでは`date`コマンドを使って、未来または過去の日付を計算します。以下に例を示します。

```fish
# 今日から10日後の日付を計算する
set future_date (date -d "+10 days" +"%Y-%m-%d")
echo $future_date

# 出力例: 2023-04-14

# 今日から10日前の日付を計算する
set past_date (date -d "-10 days" +"%Y-%m-%d")
echo $past_date

# 出力例: 2023-03-25
```

## Deep Dive (深掘り)

過去には、日付を計算するために独自のスクリプトやアルゴリズムを書くことが一般的でした。しかし、UNIX系のオペレーティングシステムでは`date`コマンドがその役割を担ってきました。Fish Shellでは、他のシェルスクリプト同様にこのコマンドを使って作業を行いますが、構文が簡単で読みやすいことが特徴です。

代替手段としては、GNU `date`の代わりに`datetime`モジュールを使用するPythonスクリプトなどが挙げられます。これにはより高度な計算やタイムゾーンのサポートが含まれています。

実装の詳細では、`date`コマンドは、現在のシステム時刻を基に加減算された秒数によって日付を計算します。加えたい日数や時間を指定する際の`+`や`-`は、それぞれ未来や過去への相対時間を意味します。

## See Also (関連情報)

- Fish Shellの公式ドキュメント: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- GNU `date`コマンドのマニュアル: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Python datetimeモジュール: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
