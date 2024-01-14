---
title:                "Python: 現在の日付を取得する"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

Pythonを学んだり、使用したりしている皆さんは、日々現在の日付を取得する必要があることを知っています。例えば、今日の日付を表示したい、あるいは特定の日付と現在の日付を比較したい場合があります。この記事では、Pythonを使って現在の日付を取得する方法を解説します。

## 方法

Pythonには、現在の日付を取得するための便利なモジュールがあります。それが`datetime`モジュールです。まずは、このモジュールをインポートしましょう。

```python
import datetime
```

次に、`datetime`モジュールから`today()`メソッドを使って現在の日付を取得します。そして、`strftime()`メソッドを使って日付を指定したフォーマットに変換します。例えば、次のようなコードになります。

```python
current_date = datetime.today().strftime('%Y-%m-%d')
print(current_date)
```

このコードを実行すると、現在の日付が`YYYY-MM-DD`のフォーマットで表示されます。

さらに、日付を比較することもできます。例えば、次のようなコードを使って、現在の日付が指定した日付よりも後かどうかを判定することができます。

```python
specified_date = datetime.date(2021, 5, 1)
if datetime.today().date() > specified_date:
    print('現在の日付は指定した日付よりも後です。')
else:
    print('現在の日付は指定した日付よりも前です。')
```

このように、Pythonを使って現在の日付を取得することができます。

## ディープダイブ

Pythonの`datetime`モジュールには、日付の計算やタイムゾーンの情報を取得するための多くの便利な関数やメソッドがあります。さらに、`calendar`モジュールを使うことで曜日や月の情報を取得することもできます。これらの機能を使うことで、より高度な日付操作が可能になります。

## 参考リンク

- [Pythonで日付を扱う方法 (Qiita)](https://qiita.com/ubuntaka/items/749ce2d11984d7123e96)
- [Pythonの日付と時刻を取得する方法 (プログラミング初心者入門講座)](https://www.javadrive.jp/python/time/index8.html)
- [Pythonでカレンダーを表示する方法 (Qiita)](https://qiita.com/Kyou13/items/34a6a2d06b423d6997ba)

## おわりに

Pythonの`datetime`モジュールを使うことで、簡単に現在の日付を取得することができます。さらに、日付の計算やタイムゾーンの情報を取得することで、より便利に日付を操作することができます。ぜひこの記事を参考に、Pythonで日付を取得してみてください。

## 関連リンク

- [Markdown記法 (Qiita)](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [Python資料集 (Python.jp)](https://www.python.jp/text/)