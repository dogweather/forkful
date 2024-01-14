---
title:                "Python: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

計算は、私たちが日々の活動を効率的に行うのに不可欠です。あなたが正確な日付を知る必要がある場合、プログラミング言語を使用してそれを計算することは非常に役立ちます。過去や将来の日付を計算することは、特に予定やタイムラインを追跡する必要がある場合に役立ちます。

## 方法

まず、datetimeモジュールをインポートします。このモジュールには、日付や時刻を操作するための便利な機能があります。

```Python
import datetime
```

次に、任意の日付をdatetimeオブジェクトとして作成します。例えば、今日の日付は次のように作成できます。

```Python
today = datetime.date.today()
print(today)
```

これにより、現在の日付が出力されます。同様に、明日の日付を計算することもできます。

```Python
tomorrow = today + datetime.timedelta(days=1)
print(tomorrow)
```

このように、timedeltaを使用することで、任意の日数を加算することができます。

そして、過去の日付を計算するには、マイナスの日数を加算すれば良いです。例えば、1ヶ月前の日付を計算する場合は次のようになります。

```Python
one_month_ago = today - datetime.timedelta(days=30)
print(one_month_ago)
```

## ディープダイブ

日付を計算する方法は他にもたくさんあります。例えば、datetimeクラスのstrftimeメソッドを使用することで、日付のフォーマットを指定することができます。また、日付の比較や演算も可能です。詳しくは、公式ドキュメントを参照してください。

## 関連リンク

- [Python公式ドキュメント - datetimeモジュール](https://docs.python.org/ja/3/library/datetime.html)
- [Pythonプログラミングによる日付の操作方法](https://www.soudegesu.com/python/datetime/)
- [Pythonで日付を計算する方法](https://qiita.com/zaramme/items/7a0cb10ff6f862744827)