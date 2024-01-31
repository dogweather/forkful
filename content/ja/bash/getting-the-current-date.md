---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:12:56.792615-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (なぜとは？)
プログラマは日付データをよく使います。ログ作成、スケジューリング、有効期限の管理などに現在日付を取得することが必要です。

## How to: (方法)
```Bash
# 現在の日付を表示
date "+%Y-%m-%d"
```

出力例:
```
2023-04-05
```

```Bash
# 時間も含めて表示
date "+%Y-%m-%d %H:%M:%S"
```

出力例:
```
2023-04-05 15:21:30
```

## Deep Dive (詳細情報)
UNIX系システムでは、1970年1月1日を初めとするエポックタイムからの経過時間で日付が計算されます。`date` コマンドはこのエポックタイムを元に現在日付を提供します。他の方法として `awk` や `perl` を使うことも可能ですが、`date` コマンドが最も直接的で簡単です。スクリプト内で `$(date)` 構文を使用し、コマンドの出力を変数に代入し操作を行うことが一般的です。タイムゾーンやロケールによって表示形式が変わることも重要な点です。

## See Also (関連情報)
- GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Bash Scripting Tutorial: https://ryanstutorials.net/bash-scripting-tutorial/
- Epoch & Unix Timestamp Conversion Tools: https://www.epochconverter.com/
