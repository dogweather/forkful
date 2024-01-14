---
title:                "Bash: 「現在の日付を取得する」"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
Bashプログラミングをする人の中には、現在の日付を取得したいというニーズを持っている人が多いでしょう。例えば、スクリプトを作成して現在の日付をファイル名に使用したい場合などには、Bashで現在の日付を取得する必要があります。

## 方法
日付を取得するためには、Bashの組み込みコマンドである`date`コマンドが使用されます。以下の例を参考にしてください。

```Bash
#!/bin/bash

# 現在の日付をフォーマットする
date +"%Y-%m-%d"

# 現在の日付と時刻をフォーマットする
date +"%Y-%m-%d %H:%M:%S"

# タイムゾーンを指定して現在の日付を取得する
TZ='Asia/Tokyo' date +"%Y-%m-%d %H:%M:%S"
```

上記のスクリプトを実行すると、現在の日付が指定したフォーマットで表示されます。

## 深堀り
`date`コマンドを使用すると、現在の日付以外にもさまざまな情報を取得することができます。例えば、現在の曜日や月、季節、週数、ユニークなタイムスタンプなどを取得することができます。また、タイムゾーンやロケールを指定することで、ローカルな日付を取得することも可能です。

## さらに参考に
- [Bashのdateコマンドのマニュアル](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Bashのローカル日付とタイムゾーンを設定する方法](https://blog.kdolph.in/2018/03/30/configuring-bash-localization-adding-local-timestamp/)
- [Bashでファイルに現在の日付を付けて保存する方法](https://linuxhandbook.com/timestamp-bash/)