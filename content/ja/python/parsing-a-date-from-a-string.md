---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から日付を解析するとは、文字列形式のデータを日付形式に変換することを意味します。これは、プログラマが文字列情報を特定の形式の日付として操作する必要があるときに行われます。

## どうやって:

Pythonの`datetime`モジュールの`strptime`関数を使います。見てみましょう：

```Python
from datetime import datetime

date_string = "2022年4月5日"
date_object = datetime.strptime(date_string, "%Y年%m月%d日")

print(date_object)
```

これは、以下の出力を表示します：

```Python
2022-04-05 00:00:00
```

ここで、`strptime`関数は文字列を日付オブジェクトに変換します。第二パラメータは特定の日付形式を示します。

## 深掘り：

1. 歴史的な文脈: Pythonで日付を解析するための関数はPythonの初期バージョンから存在しています。しかし、現在版であるPython 3.8以降では、より簡単な解析のために新しい関数が導入されています。

2. 代替案: `dateutil.parser`は自動的に日付を解析するオプションを提供します。

```Python
from dateutil.parser import parse

date_string = "2022年4月5日"
date_object = parse(date_string)

print(date_object)
```

3. 実装詳細: `strptime`は内部的にCライブラリの関数を使用していて効率的です。ただし日付形式を間違えるとエラーが返ってきます。

## 参照：

- Python公式ドキュメントに`datetime`が詳しく解説されています：https://docs.python.org/3/library/datetime.html

- dateutil.parserの使用についてはこちらをご覧ください：https://dateutil.readthedocs.io/en/stable/parser.html