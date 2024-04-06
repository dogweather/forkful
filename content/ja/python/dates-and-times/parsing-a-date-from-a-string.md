---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:07.163680-07:00
description: "\u65B9\u6CD5 Python\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\
  \u306F\u3001\u3053\u306E\u76EE\u7684\u306E\u305F\u3081\u306E`strptime`\u30E1\u30BD\
  \u30C3\u30C9\u3092\u542B\u3080`datetime`\u30E2\u30B8\u30E5\u30FC\u30EB\u304C\u63D0\
  \u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u3053\u306E\u30E1\u30BD\u30C3\u30C9\
  \u306B\u306F\u3001\u65E5\u4ED8\u6587\u5B57\u5217\u3068\u5165\u529B\u6587\u5B57\u5217\
  \u306E\u30D1\u30BF\u30FC\u30F3\u3092\u6307\u5B9A\u3059\u308B\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u6307\u793A\u5B50\u306E2\u3064\u306E\u5F15\u6570\u304C\u5FC5\u8981\u3067\
  \u3059\u3002"
lastmod: '2024-04-05T22:37:49.846244-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5 Python\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\
  \u306F\u3001\u3053\u306E\u76EE\u7684\u306E\u305F\u3081\u306E`strptime`\u30E1\u30BD\
  \u30C3\u30C9\u3092\u542B\u3080`datetime`\u30E2\u30B8\u30E5\u30FC\u30EB\u304C\u63D0\
  \u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u3053\u306E\u30E1\u30BD\u30C3\u30C9\
  \u306B\u306F\u3001\u65E5\u4ED8\u6587\u5B57\u5217\u3068\u5165\u529B\u6587\u5B57\u5217\
  \u306E\u30D1\u30BF\u30FC\u30F3\u3092\u6307\u5B9A\u3059\u308B\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u6307\u793A\u5B50\u306E2\u3064\u306E\u5F15\u6570\u304C\u5FC5\u8981\u3067\
  \u3059\u3002"
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
