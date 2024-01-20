---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？
現在の日付を取得するとは、コンピュータシステムが現在の日時を特定するプロセスです。これは日々のロギング、時刻依存の機能、または日付を使ったソフトウェア更新などを行うためにプログラマーによって行われます。

## 実行方法：
Pythonで現在の日付を取得するのは非常に直感的で、以下の`datetime`モジュールを使います：

```Python
from datetime import date
today = date.today()
print("Today's date:", today)
```

これを実行すると、出力は以下のようになります：

```Python
Today's date: 2022-01-22
```

## 深堀り：
`datetime`モジュールはPythonの標準ライブラリの一部で、日付と時間を操作するためのクラスを提供しています。将来的には、`datetime`モジュールに代わる方法が存在するかもしれません。例えば、`time`モジュールはより低レベルの時間関連の操作に使用されますが、日付を直接取得するには`datetime`モジュールが最も適しています。

取得された日付の具体的な形式はシステムのロケール設定とPythonのバージョンに依存します。デフォルトでは、日付は「YYYY-MM-DD」形式の文字列として返されます。

## 参照：
- Python公式ドキュメンテーションの [datetimeモジュール](https://docs.python.org/ja/3/library/datetime.html)
- Python公式ドキュメンテーションの [timeモジュール](https://docs.python.org/ja/3/library/time.html)
- Python日付と時刻の取扱の詳細についての [参考記事](https://realpython.com/python-datetime/)