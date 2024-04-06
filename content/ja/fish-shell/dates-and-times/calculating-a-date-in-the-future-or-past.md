---
date: 2024-01-20 17:31:10.426870-07:00
description: "How to: (\u3084\u308A\u65B9) Fish Shell\u3067\u306F`date`\u30B3\u30DE\
  \u30F3\u30C9\u3092\u4F7F\u3063\u3066\u3001\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\
  \u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\
  \u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.537055-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Fish Shell\u3067\u306F`date`\u30B3\u30DE\u30F3\u30C9\
  \u3092\u4F7F\u3063\u3066\u3001\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\
  \u4ED8\u3092\u8A08\u7B97\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\
  \u3057\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

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
