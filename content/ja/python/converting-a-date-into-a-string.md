---
title:                "Python: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することの意義は、日付をわかりやすく処理したり、ファイル名やデータベースのエントリとして使用するためです。

## 方法
```
Python # 日付を文字列に変換する
import datetime

# 日付オブジェクトの作成
today = datetime.date.today()

# 文字列に変換
date_string = today.strftime("%Y-%m-%d")

# 出力
print(date_string) # "2021-08-21"
```

## 深堀り
日付を文字列に変換する際、フォーマット指定によって表示される文字列の形式を変えることができます。例えば、`"%Y/%m/%d"`と指定すると「年/月/日」の形式で表示されます。また、日本語の曜日を表示するには`"%Y年%m月%d日 %A"`と指定することができます。

## 参考
- [Pythonの日付処理](https://www.javadrive.jp/python/date/index1.html)
- [Pythonのstrftimeメソッドを使って日付をフォーマットする](https://www.sejuku.net/blog/88276)
- [Pythonのdatetime.date.today()で今日の日付を取得する方法](https://qiita.com/furr/items/0e318987f267b75a6bba)