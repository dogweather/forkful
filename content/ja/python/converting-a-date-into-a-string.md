---
title:    "Python: 日付を文字列に変換する"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由を説明します。日付を文字列に変換することで、コードによって処理をさせることができるようになります。

## 方法
日付を文字列に変換する方法は、Pythonの組み込み関数の`str`を使用します。以下の例は、今日の日付を文字列に変換する方法を示しています。

```Python
from datetime import date

today = date.today()
str_date = str(today) #今日の日付を文字列に変換
print(str_date)
```

出力: `2021-01-01`

## 深堀り
日付を文字列に変換する際、フォーマットを指定することもできます。`strftime()`メソッドを使用することで、指定したフォーマットに従った文字列を作成することができます。例えば、以下のコードは年月日のフォーマットで日付を文字列に変換する方法を示しています。

```Python
from datetime import date

today = date.today()
str_date = today.strftime("%Y年%m月%d日") #年月日のフォーマットで日付を文字列に変換
print(str_date)
```

出力: `2021年01月01日`

## はてな
日付を文字列に変換する方法は、プログラミングでよく使用されるテクニックの一つです。日付を文字列に変換することで、データの処理や表示をより柔軟に行うことができます。

## 関連リンク
- [Python公式ドキュメント: `datetime`モジュール](https://docs.python.org/ja/3/library/datetime.html)
- [Python公式ドキュメント: `str`関数](https://docs.python.org/ja/3/library/functions.html#func-str)
- [Python公式ドキュメント: `strftime()`メソッド](https://docs.python.org/ja/3/library/datetime.html#datetime.datetime.strftime)