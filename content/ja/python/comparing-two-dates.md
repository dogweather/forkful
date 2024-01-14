---
title:                "Python: 「日付の比較」"
simple_title:         "「日付の比較」"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜ？

日付を比較する理由は様々ありますが、一般的なケースとしては、例えばバースデーや記念日の計算や、ある日が別の日よりも前か後ろかを判断するためなどが挙げられます。Pythonを使えば、簡単に日付を比較することができます。

## 使い方

Pythonで日付を比較するには、まずdatetimeモジュールをインポートします。次に、比較したい日付をdatetimeオブジェクトとして定義します。そして、比較演算子を使って日付を比較します。例えば、以下のように書くことができます。

```Python
import datetime

date1 = datetime.datetime(2020, 5, 12)
date2 = datetime.datetime(2020, 5, 10)

if date1 > date2:
  print("Date1 is later than Date2")
```

上記の例では、date1がdate2よりも後の日付であるため、"Date1 is later than Date2"というメッセージが出力されます。

また、日付を比較する際には、平日や休日の判定も行うことができます。datetimeオブジェクトのweekday()メソッドを使うことで、1が月曜日、6が土曜日、7が日曜日を表すことができます。例えば、以下のように書くことができます。

```Python
import datetime

date = datetime.datetime.today()

if date.weekday() < 5:
  print("Today is a weekday")
```

上記の例では、今日が平日である場合、"Today is a weekday"というメッセージが出力されます。

## 深堀り

Pythonのdatetimeモジュールには、日付を扱うためのさまざまな機能があります。例えば、日付のフォーマットを変更するstrptime()やstrftime()メソッド、日付の足し算や引き算を行うtimedeltaオブジェクトなどがあります。これらの機能を使うことで、より複雑な日付の比較や計算が可能になります。

また、Python以外にも、日付を比較するためのライブラリやツールがあります。例えば、dateutilやPendulumなどが挙げられます。これらのツールを使うことで、より柔軟に日付を扱うことができるようになります。

# さらに見るもの

- [Python公式ドキュメント - 高度な日付と時刻の処理](https://docs.python.org/ja/3/library/datetime.html)
- [Python公式ドキュメント - datetimeモジュール](https://docs.python.org/ja/3/library/datetime.html)
- [dateutil](https://pypi.org/project/python-dateutil/)
- [Pendulum](https://pendulum.eustace.io/)