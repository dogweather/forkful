---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:07.163680-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.512559-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u65E5\u4ED8\u304A\u3088\u3073\u6642\
  \u523B\u60C5\u5831\u3092datetime\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u307E\u305F\
  \u306F\u540C\u7B49\u306E\u69CB\u9020\u5316\u3055\u308C\u305F\u5F62\u5F0F\u306B\u5909\
  \u63DB\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u3053\u308C\u306F\
  \u3001\u65E5\u4ED8\u306E\u7B97\u8853\u3001\u6BD4\u8F03\u3001\u304A\u3088\u3073\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u64CD\u4F5C\u3092\u8A00\u8A9E\u3084\u5730\u57DF\u306B\
  \u4F9D\u5B58\u3057\u306A\u3044\u65B9\u6CD5\u3067\u884C\u3046\u305F\u3081\u306B\u4E00\
  \u822C\u7684\u306B\u884C\u308F\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30ED\u30B0\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3001\u307E\
  \u305F\u306F\u5916\u90E8\u30BD\u30FC\u30B9\u304B\u3089\u62BD\u51FA\u3057\u305F\u6642\
  \u9593\u30C7\u30FC\u30BF\u3092\u52B9\u7387\u7684\u306B\u6271\u3044\u3001\u64CD\u4F5C\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 方法
Pythonの標準ライブラリには、この目的のための`strptime`メソッドを含む`datetime`モジュールが提供されています。このメソッドには、日付文字列と入力文字列のパターンを指定するフォーマット指示子の2つの引数が必要です。

```python
from datetime import datetime

# 例の文字列
date_string = "2023-04-01 14:30:00"
# 文字列をdatetimeオブジェクトに解析する
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# 出力: 2023-04-01 14:30:00
```

特に複数の形式やロケールを扱う場合には、より微妙な日付の解析が必要になることがあります。その場合、サードパーティのライブラリ`dateutil`が非常に役立ちます。これは、ほぼ任意の文字列形式の日付を解析できるパーサーモジュールを提供します。

```python
from dateutil import parser

# 例の文字列
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# dateutilのパーサーを使用
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# 出力: 2023-04-01 14:30:00
print(parsed_date2)
# 出力: 2023-04-01 14:30:00
```

`dateutil`は、明示的なフォーマット文字列なしでほとんどの日付形式を扱うことが得意であり、多様な日付表現を扱うアプリケーションにとって汎用的な選択肢となります。
