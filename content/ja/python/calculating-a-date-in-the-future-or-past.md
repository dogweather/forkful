---
title:                "Python: 将来または過去の日付の計算"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ計算するのか？

時と日付の計算は、将来や過去の特定の日付を知りたい場合や、イベントの予定を立てる場合などに役立ちます。Pythonプログラミングを使用することで、これらの計算をワンクリックで簡単に行うことができます。

## 計算する方法

まず第一に、Pythonのdatetimeモジュールをインポートする必要があります。これは、日付や時間に関するさまざまなデータを管理するための便利なツールを提供してくれます。

次に、計算したい日付を変数に設定します。例えば、1ヶ月後の日付を計算したい場合は、``current_date = datetime.date.today()``と入力します。

そして、計算したい期間を設定します。場合によっては、月や日単位の期間を計算したいこともあるかもしれません。その場合は、``delta = datetime.timedelta(months=1)``のように入力します。

最後に、``result = current_date + delta``と入力して計算を行い、``print(result)``で結果を出力します。これで、指定した日付から1ヶ月後の日付が出力されます。

```Python
import datetime

current_date = datetime.date.today()
delta = datetime.timedelta(months=1)

result = current_date + delta
print(result)
```
出力結果:
 
2021-03-31

## 詳細を深める

さらに複雑な計算を行うには、計算したい日付を変数として指定することができます。例えば、あるイベントが予定されている日を知りたい場合は、その日付を変数に設定し、現在の日付を入力して差を求めることができます。

また、逆に、指定した日付から過去の日付を求めることもできます。例えば、「1ヶ月前には何日だったか」といった計算も可能です。

また、Pythonのdatetimeモジュールには、カレンダーやタイムゾーンに関する機能もありますので、更に詳しく日付や時間を計算したい場合は、公式ドキュメントなどを参考にしてみてください。

## 参考リンク

- Pythonの公式ドキュメント: https://www.python.org/
- datetimeモジュールのドキュメント: https://docs.python.org/3/library/datetime.html
- Pythonで日付や時間を扱う方法: https://www.digitalocean.com/community/tutorials/how-to-work-with-datetime-objects-in-python-3-ja