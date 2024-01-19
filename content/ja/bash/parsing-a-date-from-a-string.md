---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から日付を解析するとは、文字列形式で表現された日付と時刻をコンピューターが理解できる日付オブジェクトに変換することです。これは、日付を比較したり、計算したりする為にをプログラマーが行います。

## どうやってやるか：

以下にBashで文字列から日付を解析するサンプルコードを示します：

```Bash
date -d '2022-01-01 00:00:00' '+%s'
```

実行結果は以下の通りです：

```Bash
1640995200
```

このコードは、指定した日付と時刻をUNIXエポック（1970年1月1日）からの経過秒数で表す形式に変換しています。


## ディープダイブ：

Bashにおける日付パースの機能は、UNIXライクなオペレーティングシステムの歴史と密接に関連しています。UNIXエポックは、UNIX、Linux、そしてその他多くのシステムで使われる時間の基準点です。

代替方法として、GNU `date`コマンドの機能を活用することも可能です。ユーザーフレンドラーな日付形式をパースできるのが特徴です。

Bashが日付を処理する方法には、コンピュータのローカルタイムゾーンが考慮される点があります。そのため、タイムゾーンの違う場所で同じコードを実行すると、出力結果が変わる可能性があります。

## 関連情報：

以下のサイトも参考にしてみてください。

GNU coreutils - Dateコマード：https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html

UNIXエポック時間とは何か：https://ja.wikipedia.org/wiki/UNIX%E6%99%82%E9%96%93

Bashスクリプトについての追加情報：https://www.tldp.org/LDP/abs/html/abs-guide.html