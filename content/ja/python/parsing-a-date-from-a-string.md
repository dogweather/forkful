---
title:                "文字列から日付を解析する"
html_title:           "Python: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何をするの？
日付文字列を解析することは、日付の情報を含む文字列から日付を抽出することです。プログラマーがこれを行うのは、日付情報をより効率的に処理するためです。

## 方法：
```
# 日付の文字列から日付を解析する方法
import datetime

# 日付の文字列の定義
date_string = '2021年1月1日'

# 文字列を日付に変換する
parsed_date = datetime.datetime.strptime(date_string, '%Y年%m月%d日')

print(parsed_date.date()) # 2021-01-01
```

## 詳細説明：
日付を含む文字列から日付を抽出するというアイデアは、プログラミングの初期から存在しています。古くはプログラム言語に含まれておらず、自分で関数を書く必要がありましたが、今では多くの言語に組み込まれています。代表的な方法として、パターンマッチングや正規表現を使用して日付を抽出する方法があります。

## 参考情報：
- [Pythonのdatetimeモジュール](https://docs.python.org/ja/3/library/datetime.html)
- [日付文字列を解析する方法についての詳しい説明](https://realpython.com/python-datetime/#parsing-dates-from-strings)