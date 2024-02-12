---
title:                "文字列から日付をパースする"
aliases:
- ja/python/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:07.163680-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## はじめに
文字列から日付を解析するとは、テキストの日付および時刻情報をdatetimeオブジェクトまたは同等の構造化された形式に変換することを指します。これは、日付の算術、比較、およびフォーマット操作を言語や地域に依存しない方法で行うために一般的に行われます。プログラマーは、ログ、ユーザー入力、または外部ソースから抽出した時間データを効率的に扱い、操作するためにこれを行います。

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
