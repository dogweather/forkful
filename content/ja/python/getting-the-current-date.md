---
title:                "現在の日付の取得"
html_title:           "Python: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 今回の記事の目的：今日の日付をPythonで取得する方法

## 何となぜ？
日付を取得することは、プログラマーにとって非常に便利です。日付を取得することにより、アプリケーションで現在の日付を表示したり、特定のタイムゾーンの日付を取得したりすることができます。

## 方法：
Pythonでは、日付を取得するための標準モジュールとして `datetime` を使用することができます。以下のようにコードを書くことで、現在の日付を取得することができます。

```Python
import datetime

today = datetime.date.today()

print(today)
```

上記のコードを実行すると、今日の日付が `YYYY-MM-DD` の形式で出力されます。

## 深堀り：
Pythonでは、 `datetime` モジュール以外にも日付を取得するための方法があります。例えば、 `calendar` モジュールの `timegm` 関数を使用することで UNIX タイムスタンプから日付を取得することができます。

また、Python以外にも `moment.js` というJavaScriptのライブラリも使うことができます。このライブラリでは、日付のフォーマットやタイムゾーンの変更なども簡単に行うことができます。

さらに、Pythonでは `datetime` モジュールを使って日付の他にも時刻やタイムゾーンの情報を取得することができます。詳細は公式ドキュメントを参照してください。

## 関連リンク：
- [Python 公式ドキュメント](https://docs.python.org/ja/3.8/library/datetime.html)
- [moment.js ライブラリ](https://momentjs.com/)