---
title:                "未来または過去の日付を計算する"
html_title:           "Python: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？
未来または過去の日付の計算は、所定の期間を追加または減算して特定の日付を得るプログラミング処理です。これは予定の日付を自動化するため、または特定の日付と現在日付との間の時間差を求めるなど、多くの現実世界の問題を解決するために必要です。

## 使い方：
以下にPythonの基本的な日付操作のコード例を示します。

```Python
from datetime import datetime, timedelta

# Current Date
current_date = datetime.now()

print(f"Current date: {current_date}")

# Add one day
one_day = timedelta(days=1)
future_date = current_date + one_day

print(f"Date tomorrow: {future_date}")

# Subtract one day
past_date = current_date - one_day

print(f"Date yesterday: {past_date}")
```
このコードを実行すると、現在の日付、明日の日付、そして昨日の日付がそれぞれ出力されます。

## ディープダイブ
計算日付は古代からの問題で、月と日、年と日の間の不一致を扱う必要がありました。Pythonではこの問題を`datetime`と`timedelta`モジュールで解決しています。これは`timedelta`オブジェクトを`datetime`オブジェクトに追加または引くことで、「未来」または「過去」の日付を取得します。

代替手段としては、`dateutil`ライブラリがあります。`dateutil.relativedelta`はPythonの`timedelta`よりも高機能であり、月や年の単位で日付の差を取ることができます。

その実装の詳細については、`datetime`と`timedelta`はPythonの組み込みライブラリであり、Pythonのソースコードに直接実装されています。

## 関連リンク
- Python公式ドキュメント: [datetime](https://docs.python.org/ja/3/library/datetime.html) 
- Python公式ドキュメント: [timedelta](https://docs.python.org/ja/3/library/datetime.html#timedelta-objects)
- dateutilライブラリ: [relativedelta](https://dateutil.readthedocs.io/en/stable/relativedelta.html)