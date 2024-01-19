---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付を文字列に変換するとは、日付のデータタイプを文字列のデータタイプに変更することです。プログラマーがこの変換を行う理由は、日付を人間が読める形式に変換したり、ログとして出力したりするためです。

## 方法：
以下に、日付を文字列に変換するBashコード例とその出力結果を示します。

```Bash
current_date=`date +%Y-%m-%d`
echo $current_date
```

上記のスクリプトは現在の日付を "年-月-日" 形式で出力します。例えば '2021-05-10' のような形式です。

## ディープダイブ：
### 歴史的文脈：
UNIXシェルスクリプトにおいて、日付と時間の操作は常に重要な部分を占めています。特に、ログファイルやレポートの生成において、日付を文字列に変換することが需給されます。

### 代替案：
他の言語、例えばPythonでも日付を文字列に変換することは可能です。また、日付形式を扱うための専用のツールもあります。

```Python
from datetime import date
current_date = str(date.today())
print(current_date)
```

### 実装の詳細：
`date +%Y-%m-%d`コマンドは現在の日付を出力します。`+`オプションとそれに続くパラメータは出力形式を定義します。

## 参考になるリンク：
1. [Advanced Bash-Scripting Guide: Date and Timestamp](http://www.tldp.org/LDP/abs/html/timedate.html)
2. [Unix StackExchange: How to convert date to string](https://unix.stackexchange.com/questions/72139/how-do-i-convert-date-to-a-string)
3. [Python documentation: datetime module](https://docs.python.org/3/library/datetime.html)