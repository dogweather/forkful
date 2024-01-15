---
title:                "将来または過去の日付の計算"
html_title:           "Python: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を計算する理由は、将来または過去の日付を特定する必要があるためです。例えば、予定や重要なイベントを計画する場合、特定の日付を知ることが重要です。

## 方法

```python
import datetime # datetimeモジュールをインポート

# 今日の日付を取得
today = datetime.date.today()

# １週間後の日付を計算
one_week = today + datetime.timedelta(days=7)

print(one_week) # 今日から１週間後の日付を出力 (例: 2020-09-07)
```

```python
import datetime # datetimeモジュールをインポート

# 指定した日付を設定
date = datetime.date(2020, 10, 15)

# １０日前の日付を計算
ten_days_ago = date - datetime.timedelta(days=10)

print(ten_days_ago) # １０日前の日付を出力 (例: 2020-10-05)
```

## ディープダイブ

日付の計算には、datetimeモジュールのtimedeltaクラスを使用します。日数、週数、月数、年数を指定して、特定の日付を計算することができます。また、dateクラスを使用することで、指定した年月日のオブジェクトを作成することができます。

## 参考リンク

- [datetimeモジュールドキュメント](https://docs.python.org/ja/3/library/datetime.html)
- [timedeltaクラスドキュメント](https://docs.python.org/ja/3/library/datetime.html#timedelta-objects)
- [dateクラスドキュメント](https://docs.python.org/ja/3/library/datetime.html#date-objects)