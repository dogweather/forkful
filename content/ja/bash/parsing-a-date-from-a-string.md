---
title:                "文字列から日付を分析する"
html_title:           "Bash: 文字列から日付を分析する"
simple_title:         "文字列から日付を分析する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## なに？なぜ？

パースというのは、文字列から日付を抽出することです。プログラマーがこの作業を行う理由は、様々なデータ処理やデータ分析を行う際に、日付データを正確に取得する必要があるからです。日付は、データのソートや集計に欠かせない重要な要素です。

## 使い方：

以下のコードブロックに示すように、Bashを使用して文字列から日付をパースする方法を説明します。

```
# 文字列から日付を抽出する例
date_string='2021年1月1日'
parsed_date=$(date -d "${date_string}" '+%Y-%m-%d')

echo ${parsed_date} # 出力：2021-01-01
```

```
# 現在日時から日付を取得する例
parsed_date=$(date '+%Y-%m-%d')

echo ${parsed_date} # 出力：現在の日付が表示される
```

```
# 任意のフォーマットの日付をパースする例
date_string='1/1/2021'
parsed_date=$(date -d "${date_string}" '+%Y-%m-%d')

echo ${parsed_date} # 出力：2021-01-01
```

## 深堀り

日付をパースする作業は、日付の形式や表記方法が世界各国で異なるため、国際化において重要です。また、Bash以外にもPythonやJavaScriptなどのプログラミング言語でも同様の作業が可能です。

日付のパースを行う際には、正規表現を用いる方法もあります。また、現在日時を取得するには、dateコマンドの他にも時刻を取得するためのコマンドが存在します。

## 関連リンク：

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/html_node/Date-Input-Format.html)
- [Python datetimeモジュール](https://docs.python.org/ja/3/library/datetime.html)
- [日付のフォーマットを変換する方法](https://www.baeldung.com/java-date-format)