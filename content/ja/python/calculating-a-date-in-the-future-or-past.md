---
title:    "Python: 将来または過去の日付の計算"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ

日付の計算をするのは、将来や過去の特定の日付が知りたいときに便利です。

## 方法

```python
# datetime モジュールをインポート
import datetime 

# 現在の日付を取得
today = datetime.date.today() 

# 将来の日付を計算
future_date = today + datetime.timedelta(days=30)

# 過去の日付を計算
past_date = today - datetime.timedelta(days=30)

# 結果を出力
print("今日の日付: ", today) 
print("将来の日付: ", future_date) 
print("過去の日付: ", past_date)
```

出力結果:

今日の日付: 2020-07-01
将来の日付: 2020-07-31
過去の日付: 2020-05-31

## ディープダイブ

Pythonのdatetimeモジュールを使用することで、簡単に将来や過去の日付を計算することができます。また、日付の間隔を指定することもでき、例えば2週間後や3ヶ月前なども計算することができます。さらに、曜日や時間を考慮した計算も可能です。

## 参考

- [Python datetimeモジュールのドキュメント](https://docs.python.org/ja/3/library/datetime.html)
- [Pythonで日付と時間を操作する方法](https://www.javadrive.jp/python/date_manipulation/index1.html)
- [Pythonの日付操作についてのチュートリアル](https://note.nkmk.me/python-datetime-time-timedelta/)