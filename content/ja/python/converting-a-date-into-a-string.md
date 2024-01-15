---
title:                "日付を文字列に変換する"
html_title:           "Python: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することの利点や理由はいくつかあります。例えば、データベースから取得した日付を特定のフォーマットで表示したい時や、日付を人間にとって分かりやすい形式で表示したい時に役立ちます。

## 方法
```Python
# 日付を文字列に変換する方法
import datetime

# 現在の日付を取得
today = datetime.date.today()

# 文字列に変換
today_str = today.strftime("%Y年%m月%d日")

# 変換した日付を出力
print("今日は" + today_str + "です。")

# 出力結果：今日は2021年05月10日です。
```

## 詳細な説明
日付を文字列に変換するには、datetimeモジュールのstrftime()メソッドを使用します。このメソッドには、日付を指定した形式で文字列に変換するためのフォーマット済み文字列が必要です。フォーマット済み文字列には、年・月・日などの日付を表す特定の文字や記号を使用することができます。また、フォーマット済み文字列には任意の表記方法を設定することもできます。この方法を使用することで、日付をより柔軟に文字列に変換することができます。

## 参考リンク
- [Python datetimeモジュールの公式ドキュメント](https://docs.python.org/ja/3/library/datetime.html)
- [Pythonで日付を文字列に変換する方法 | Qiita](https://qiita.com/5zm/items/235199743ca386791fae)