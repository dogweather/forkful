---
title:                "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

日付を取得する理由は、日常生活でもプログラミングでも重要です。特に、特定のタスクを自動化する際に、現在の日付を取得することは便利です。例えば、日付をファイル名に追加することで、バックアップの管理がしやすくなります。

## 方法

現在の日付を取得するには、組み込みの`date`コマンドを使用します。下記の例を参考にしてください。

```Bash
$ date
Wed May 5 09:36:53 UTC 2021
```

このように、`date`コマンドを実行すると、現在の日付と時刻が表示されます。デフォルトでは、ISO8601形式の日付が表示されますが、異なるフォーマットで日付を取得することもできます。

```Bash
$ date +"%Y-%m-%d"
2021-05-05
```

`+"%Y-%m-%d"`の部分には、任意のフォーマットを指定することができます。詳しくは、`date`コマンドのマニュアルを参照してください。

## 深堀り

`date`コマンドは、GNU Coreutilsパッケージの一部です。このパッケージには、Linuxシステムの基本的なユーティリティが含まれています。`date`コマンドは、C言語で書かれたソースコードによって、現在の日付を取得しています。

`%Y`や`%m`などのフォーマット指定子は、strftime関数に由来しています。この関数は、日付や時刻を指定したフォーマットの文字列に変換することができます。

See Also

- [GNU Coreutils](https://www.gnu.org/software/coreutils/)
- [dateコマンドのマニュアル](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [strftime関数のマニュアル](https://www.man7.org/linux/man-pages/man3/strftime.3.html)