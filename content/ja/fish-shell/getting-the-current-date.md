---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:14:11.533361-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
カレントデートを取得するって、今日の日付を手に入れることです。プログラマーはロギング、タイムスタンプ、日付ベースの機能でこれを使います。

## How to: (やり方)
```Fish Shell
# 現在の日付を「年-月-日」の形式で表示
set -l today (date "+%Y-%m-%d")
echo $today
```
```
# 出力例: 2023-03-21
```

```Fish Shell
# もっと詳しい日付と時刻を取得
set -l now (date "+%Y-%m-%d %H:%M:%S")
echo $now
```
```
# 出力例: 2023-03-21 15:45:30
```

## Deep Dive (深掘り)
Fish Shellでは`date`コマンドはUnix系の伝統的なコマンドです。各OSに応じて微妙に違うけれど、基本的な使い方は同じ。他のシェル（BASHやZSH）も似たやり方ですが、Fishの構文はもっとシンプル。`date`コマンドのフォーマットオプションは多く、必要に応じてカスタマイズできます。例えば、`+%Y`は4桁の年を表示し、`+%m`は2桁の月を示す。組み合わせることで色んなフォーマットが可能。

## See Also (関連情報)
- Fishの公式ドキュメンテーション: [fishshell.com/docs](https://fishshell.com/docs/current/index.html)
- `date`コマンドについてのGNUの説明: [GNU Coreutils `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- 日付フォーマットのカスタマイズについて: [strftime.org](https://strftime.org/)
