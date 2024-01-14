---
title:    "Python: 「2つの日付の比較」"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ
日常生活や仕事で、私たちは様々な日付を比較する必要があります。例えば、イベントの日付や締め切り日を確認したり、出来事の順番を把握するために日付を比較することがあります。そこで、Pythonを使って日付を比較する方法を紹介します。

## 方法
Pythonで日付を比較するには、`date`モジュールを使います。まずは、比較したい日付をそれぞれ`date`オブジェクトに変換します。次に、比較演算子を使って、日付を比較します。下記のコードを参考にしてください。

```Python
import datetime

# 比較したい日付を設定
date1 = datetime.date(2021, 8, 25)
date2 = datetime.date(2021, 8, 30)

# 日付の比較
if date1 < date2:
  print("date1はdate2よりも前の日付です。")
elif date1 > date2:
  print("date1はdate2よりも後の日付です。")
else:
  print("date1とdate2は同じ日付です。")
```

上記のコードを実行すると、`date1はdate2よりも前の日付です。`という結果が得られます。

## 深堀り
日付を比較する際に気をつけなければいけないのは、日付を文字列として比較しないことです。文字列として比較した場合、例えば`"2021/08/25" < "2021/08/30"`となりますが、実際には逆の結果になります。なぜなら、文字列として比較するときは、文字の並び順で比較されるためです。そのため、日付を比較する際には必ず`date`オブジェクトに変換してから比較するようにしましょう。

## 他にも参考になる記事
- [Pythonで日付を扱う方法](https://www.python.jp/train/date.html)
- [Pythonのdateモジュールのドキュメント](https://docs.python.org/ja/3.7/library/datetime.html#date-objects)
- [Pythonの比較演算子についての記事](https://www.python.jp/train/compare/index.html)

## 他にも参考になるリンク
- [Pythonの日付に関する公式ドキュメント (英語)](https://docs.python.org/3.7/library/datetime.html)
- [Python公式ドキュメントの日本語訳 (日本語)](https://www.python.jp/doc/)
- [Pythonの日付や時間を操作するライブラリ (英語)](https://github.com/dateutil/dateutil/)