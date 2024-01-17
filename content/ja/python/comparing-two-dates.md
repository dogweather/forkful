---
title:                "「日付の比較」"
html_title:           "Python: 「日付の比較」"
simple_title:         "「日付の比較」"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何か:? 
2つの日付を比較することは、日付の間の関係を確認することです。プログラマーたちは、様々な理由を持って2つの日付を比較する必要がありますが、一般的な理由は、プログラム内で日付を処理する必要があるためです。

## 方法:
あなたが2つの日付を比較するためのPythonのコードを書きたいなら、以下の例を参考にしてください。 

```Python
date_1 = '2021-01-01'
date_2 = '2021-01-05'

if date_1 < date_2:
  print('date_1はdate_2よりも前です。')
elif date_1 == date_2:
  print('date_1とdate_2は同じ日付です。')
else:
  print('date_1はdate_2よりも後です。')
```

このコードを実行した場合、コードブロック内のprint文によって以下のように出力されます。

```Python
date_1はdate_2よりも前です。
```

## 深堀り:
日付を比較する方法にはいくつかの方法があります。プログラムで利用可能な日付のフォーマットによっても異なります。また、日付を比較する際には、タイムゾーンや夏時間の考慮に注意する必要があります。さらに、Pythonには日付型を扱うための組み込み関数やライブラリがあります。

## 併せて参照:
- Python公式ドキュメント [https://docs.python.org/ja/3/library/datetime.html](https://docs.python.org/ja/3/library/datetime.html)
- Pythonの日付比較におけるよくある誤り [https://unicode-org.github.io/icu/userguide/datetime/date-formatting.html#specification](https://unicode-org.github.io/icu/userguide/datetime/date-formatting.html#specification) 
- タイムゾーンと夏時間に関する詳細情報[https://jp.projility.com/project-management/fun-with-python-datetime-3-the-details/](https://jp.projility.com/project-management/fun-with-python-datetime-3-the-details/)