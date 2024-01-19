---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

現在の日付を取得とは、システムの現在の日付と時刻を取得することを指します。これは、ログファイルのタイムスタンプや、特定のイベントがいつ発生したかを追跡するために、プログラマーが実行します。

## 方法：

以下にBashで現在の日付を取得する際の基本的なコードを示します：
```Bash
date
```
このコードを実行すると、出力は以下のようになります：
```Bash
Thu Mar 26 12:34:56 UTC 2020
```

日付のフォーマットを指定したい場合は、"+％Y-％m-％d"のようなフォーマットを 'date' コマンドに指定します：
```Bash
date "+%Y-%m-%d"
```
そして、出力包含の日付部分は次のようになります：
```Bash
2020-03-26
```

## ディープダイブ：

現在の日付を取得するための 'date' コマンドは、Unixの初期のバージョンから存在し、GNUプロジェクトによって改善されました。他の方法として、 'printf' 関数または 'strftime' 関数を使用することも可能です。

また、'date' コマンドは内部的に 'time' システムコールを使用して現在の日時を取得します。これは、システムクロックを読み取り、結果をUTC（協定世界時）で返します。

## 参考に：

関連するリソースを以下にリンクします。

1. [GNU date command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
2. [Unix time system call](http://man7.org/linux/man-pages/man2/time.2.html)
3. [POSIX strftime function](http://pubs.opengroup.org/onlinepubs/007904975/functions/strftime.html)