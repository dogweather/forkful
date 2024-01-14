---
title:    "Python: 「現在の日付を取得する」"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 今日の日付を取得するには

最近、プログラミングの世界ではPythonがますます人気を集めています。この言語は、初心者にも優しく、覚えやすいシンタックスでプログラムを書くことができます。しかし、Pythonが提供する便利な機能の一つである日付を取得する方法は、多くの人が知らないかもしれません。今日のブログ投稿では、Pythonで現在の日付を取得する方法について説明していきます。

## なぜ？

日付を取得するということは、コンピュータプログラムでよく行われるタスクの一つです。例えば、ユーザーが何かを予約する際には、予約された日付を取得する必要があります。また、特定の日付に何かを実行するようにプログラムを設定したい場合も、日付を取得する必要があります。つまり、日付を取得することは、より便利で正確なプログラムを作るために必要不可欠なのです。

## 使い方

Pythonで日付を取得するには、標準ライブラリのdatetimeモジュールを使用します。具体的なコードは以下のようになります。

```Python
import datetime

today = datetime.date.today()
print(today)
```

上記のコードを実行すると、現在の日付が出力されます。もし、現在の日時を取得したい場合は、以下のように書き換えることができます。

```Python
import datetime

now = datetime.datetime.now()
print(now)
```

また、特定の形式で日付を取得することもできます。例えば、日付を「月/日/年」の形式で取得したい場合は、以下のコードを実行します。

```Python
import datetime

today = datetime.date.today()
formatted_date = today.strftime("%m/%d/%Y")
print(formatted_date)
```

このように、datetimeモジュールを使用することで、簡単に現在の日付を取得することができます。

## 深堀り

Pythonのdatetimeモジュールには、日付や時刻を操作するための多くの便利な関数やメソッドがあります。例えば、datetimeモジュールを使用することで、日付の加減算や差分を計算することもできます。

また、datetimeモジュールはタイムゾーンの扱いにも対応しています。時刻にタイムゾーン情報を追加することで、より正確な日付の管理が可能になります。

さらに、datetimeモジュールには日付や時刻の書式を変更するための機能も備わっており、それぞれの国や地域で使用されている標準的な書式に合わせることができます。

## その他のリソース

もし、より詳しくdatetimeモジュールについて学びたい方は、以下のリンクを参考にしてください。

- [Python公式ドキュメント - datetimeモジュール](https://docs.python.org/ja/3/library/datetime.html)
- [Python Quickstart - datetimeモジュール](https://www.programiz.com/python-programming/datetime)
- [datetimeモジュールを使ってみよう！ \| プログラマーへの道](https://www.headboost.jp/python/datetime.html)

## 参考文