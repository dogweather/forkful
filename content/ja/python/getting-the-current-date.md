---
title:                "現在の日付の取得"
html_title:           "Python: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ？

今日の日付を取得する理由は、多岐にわたります。例えば、ファイルを作成するときに現在の日付をファイル名に入れることで、一意の名前を付けることができたり、イベントの開催日を表示するために使用することができたりします。

## 使い方

現在の日付を取得するには、```datetime```モジュールを使用します。例えば、以下のようにコードを記述します。

```Python
import datetime

today = datetime.date.today()
print(today)

# Output: 2021-02-03
```

または、より詳細な日付と時刻を取得するためには、```datetime.now()```を使用します。

```Python
import datetime

now = datetime.datetime.now()
print(now)

# Output: 2021-02-03 12:34:56.789012
```

さらに、フォーマットを指定して日付を表示することもできます。例えば、```strftime()```関数を使用すると、日付を特定の形式に変換することができます。

```Python
import datetime

now = datetime.datetime.now()
formatted_date = now.strftime("%Y/%m/%d")
print(formatted_date)

# Output: 2021/02/03
```

## 深堀り

Pythonの```datetime```モジュールを使用することで、より柔軟に日付と時刻を取得することができます。```timedelta() ```関数を使うと、日付に対して特定の期間を加減することができます。

```Python
import datetime

today = datetime.date.today()
print(today)

# Output: 2021-02-03

previous_week = today - datetime.timedelta(days=7)
print(previous_week)

# Output: 2021-01-27
```

また、日付同士を比較することもできます。例えば、今日があるイベントの開催日より前か後かを判定することができます。詳しくは、Pythonの公式ドキュメントを参照してください。

## 関連リンク

- Python公式ドキュメント：https://docs.python.org/ja/3/library/datetime.html
- プログラミング入門サイト「Codecademy」：https://www.codecademy.com/learn/learn-python-3/modules/learn-python3-datetime/cheatsheet