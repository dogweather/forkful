---
title:    "Python: 現在の日付の取得"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ

Pythonプログラマーとして、日々コーディングをしているとき、時々コンピューターの日付や時間を使用する必要があるかもしれません。現在の日付や時刻を取得する必要がある理由はさまざまですが、一般的な理由は、データベースに日付や時刻を保存するため、ファイルやフォルダーをタイムスタンプで命名する必要がある、またはプログラムの実行が特定の日時や時刻に制限されている場合です。

## 方法

Pythonで現在の日付を取得するには、標準ライブラリである`datetime`モジュールを使用します。`datetime`モジュールには、現在の日付や時刻を取得するための便利な関数が用意されています。

例えば、以下のようにコードを記述することで、現在の日付や時刻を取得することができます。

```Python
import datetime
today = datetime.date.today()
current_time = datetime.datetime.now().time()

print(today) # 例：2021-05-01
print(current_time) # 例：19:52:30.372256
```

`datetime.date.today()`は、今日の日付を表す`datetime`オブジェクトを返します。また、`datetime.datetime.now().time()`は、現在の時刻を表す`datetime`オブジェクトの`time`属性を返します。

さらに、`datetime`モジュールには、日付や時刻をカスタマイズするための機能もあります。例えば、`strftime()`メソッドを使用することで、任意の形式で日付や時刻を表示することができます。

```Python
import datetime
today = datetime.date.today()
custom_date_format = today.strftime("%d/%m/%Y")

print(custom_date_format) # 例：01/05/2021
```

上記の例では、`strftime()`メソッドに`"%d/%m/%Y"`のフォーマットを指定しています。このフォーマットにより、日付を「日/月/年」の形式で表示することができます。詳細なフォーマット指定については、Pythonの公式ドキュメントを参照してください。

## ディープダイブ

現在の日付や時刻を扱う際、`datetime`モジュールには様々な機能があります。例えば、日付や時刻を計算したり、比較したり、タイムゾーンを変更したりすることができます。また、Pythonの外部ライブラリを使用することで、より高度な機能を実現することも可能です。しかしこの記事では、基本的な日付や時刻の取得方法を紹介することにとどめます。詳細な使い方や応用的な使い方については、公式ドキュメントやオンラインリソースを参考にしてください。

## 関連リンク

- Python公式ドキュメント：https://docs.python.org/ja/3/library/datetime.html
- 日付のフォーマット指定について：https://docs.python.org/ja/3/library/datetime.html#strftime-and-strptime-behavior