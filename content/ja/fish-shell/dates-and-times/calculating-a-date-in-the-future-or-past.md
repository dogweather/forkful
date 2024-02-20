---
date: 2024-01-20 17:31:10.426870-07:00
description: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u304B\u3089\u65E5\u6570\
  \u3092\u52A0\u3048\u305F\u308A\u5F15\u3044\u305F\u308A\u3057\u3066\u3001\u65B0\u3057\
  \u3044\u65E5\u4ED8\u3092\u5F97\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\
  \u30FC\u30EA\u30F3\u30B0\u3001\u671F\u9650\u306E\u8FFD\u8DE1\u3001\u307E\u305F\u306F\
  \u904E\u53BB\u306E\u30C7\u30FC\u30BF\u5206\u6790\u306E\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.864118
model: gpt-4-1106-preview
summary: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u304B\u3089\u65E5\u6570\
  \u3092\u52A0\u3048\u305F\u308A\u5F15\u3044\u305F\u308A\u3057\u3066\u3001\u65B0\u3057\
  \u3044\u65E5\u4ED8\u3092\u5F97\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\
  \u30FC\u30EA\u30F3\u30B0\u3001\u671F\u9650\u306E\u8FFD\u8DE1\u3001\u307E\u305F\u306F\
  \u904E\u53BB\u306E\u30C7\u30FC\u30BF\u5206\u6790\u306E\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
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
