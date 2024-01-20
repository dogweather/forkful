---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何＆なぜ?

日付を文字列に変換するとは、日付データを人間が読める形式に変えることです。プログラマーがこれを行う理由は、日付情報をより短い形式で煩雑さなく表示したり、日付を一般的なフォーマットで保存したい場合があるからです。

## 方法:

Pythonの`datetime`モジュールの`strftime`メソッドを使って日付を文字列に変換します。以下に例を示します。

```Python
from datetime import datetime

# 現在の日時を取得
now = datetime.now()

# 日付を文字列に変換
date_string = now.strftime("%Y-%m-%d %H:%M:%S")
print(date_string)
```

これを実行すると、次のような出力が得られます。

```Python
'2022-07-12 14:30:00'
```

この結果は、年-月-日 時:分:秒の形式で日付を表示しています。

## 深掘り:

歴史的に見て、Pythonの`strftime`関数はC言語ライブラリから派生したもので、その名前は"string format time"を意味します。フォーマット文字列は自由に指定可能で、出力する日付/時間情報をカスタマイズできます。

一方で日付を文字列に変換するPythonには他の方法もあります。例えば、`isoformat`メソッドを使用するとISO 8601形式の文字列を取得できます。

```Python
from datetime import datetime

now = datetime.now()
date_string = now.isoformat()

print(date_string)
```

これを実行すると、次のような出力が得られます。

```Python
'2022-07-12T14:30:00.000123'
```

## 参考情報:

日付と時間のフォーマットについての詳細は、Python公式ドキュメンテーションを参照してください:

- [`strftime`と`strptime`の振る舞い](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)
- [日付/時間オブジェクトに関するドキュメンテーション](https://docs.python.org/3/library/datetime.html)

他の日付と時間に関するPythonの機能も参照してみてください:

- [Pythonでの日付と時間の処理](https://docs.python.org/3/library/datetime.html)
- [Pythonの`date`オブジェクト](https://docs.python.org/3/library/datetime.html#date-objects)
- [Pythonの`time`オブジェクト](https://docs.python.org/3/library/datetime.html#time-objects)