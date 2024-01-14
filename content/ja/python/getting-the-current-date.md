---
title:                "Python: 「現在の日付の取得」"
simple_title:         "「現在の日付の取得」"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# なぜ今日の日付を取得する必要があるのか？

Pythonの日付取得メソッドは、日付や時間に関する処理を行う上で非常に便利です。例えば、特定の日付のデータを取得したり、プログラム内でファイル名に日付を付けたりする場合に役立ちます。

## 取得方法

Pythonでは、標準ライブラリの「datetime」モジュールを使用することで、簡単に現在の日付を取得することができます。以下のコードを実行すると、今日の日付を取得することができます。

```Python
import datetime

today = datetime.date.today()

print(today)
```
実行結果：

```2020-05-04```

また、日付を任意の形式で取得することも可能です。例えば、年月日の順番に「/」を挟んで表示させたい場合は、以下のようにコードを修正します。

```Python
import datetime

today = datetime.date.today()

print(today.strftime("%Y/%m/%d"))
```
実行結果：

```2020/05/04```

## 深く掘り下げる

日付取得のメソッドは、PythonのDatetimeオブジェクトとして取得されます。これにより、日付や時間に関する様々な操作が可能になります。例えば、現在の日付と一週間後の日付を比較することができます。

*今日は何曜日かを取得する方法*

```Python
import datetime

today = datetime.date.today()

print(today.strftime("%A"))

```
実行結果：

```Monday```

*一週間後の日付を取得する方法*

```Python
import datetime

today = datetime.date.today()
one_week_later = today + datetime.timedelta(days=7)

print(one_week_later)
```
実行結果：

```2020-05-11```

## 参考リンク

- [Python datetimeモジュール公式ドキュメント](https://docs.python.org/ja/3/library/datetime.html)
- [Python Datetimeオブジェクトの操作方法](https://note.nkmk.me/python-datetime-time-delta/)
- [Python Datetimeで日付を任意の形式で取得する方法](https://note.nkmk.me/python-datetime-timestamp/)
- [Python Datetimeオブジェクトを使用して一週間後の日付を取得する方法](https://note.nkmk.me/python-datetime-timedelta/)
- [Python Datetimeオブジェクトを使用して曜日を取得する方法](https://note.nkmk.me/python-datetime-weekday/)