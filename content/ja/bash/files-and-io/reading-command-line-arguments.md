---
title:                "コマンドライン引数の読み取り"
aliases: - /ja/bash/reading-command-line-arguments.md
date:                  2024-01-20T17:55:25.694615-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
コマンドライン引数の読み取りについてと、それがなぜ重要なのかをご説明します。引数はスクリプトに対して外部から情報を提供する方法です。柔軟性と再利用性を高めるために、プログラマはこれを使用します。

## How to:
コマンドライン引数を読み取り、使う方法を確認してみましょう。

```Bash
#!/bin/bash

echo "スクリプト名: $0"
echo "第1引数: $1"
echo "引数の数: $#"
echo "全引数を表示: $*"

if [ "$1" == "hello" ]; then
    echo "こんにちは！"
fi
```

実行と出力例：

```Bash
$ bash script.sh hello world
スクリプト名: script.sh
第1引数: hello
引数の数: 2
全引数を表示: hello world
こんにちは！
```

## Deep Dive
Bashでコマンドライン引数を扱う際の背景、代替手段、実装の詳細について解説します。

コマンドライン引数を扱うことはUnixの伝統であり、初期のシェルから継承されています。`$0, $1, ..., $9`のような位置パラメータを利用するだけでなく、`$@`や`shift`コマンドでより複雑な引数処理を行うこともできます。

`getopts`や`optarg`は引数とオプションを管理するための更に高度な方法を提供します。これにより、スクリプトがよりユーザーフレンドリーになります。

Bashでは、`set`や`declare`といった組み込みコマンドを利用して引数と関数内の変数を制御することも重要です。

## See Also
コマンドライン引数に関連する資料へのリンク集です。

- Bashスクリプトガイド: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- コマンドライン引数を学ぶ: https://ryanstutorials.net/bash-scripting-tutorial/bash-parameters.php
- getoptsチュートリアル: http://wiki.bash-hackers.org/howto/getopts_tutorial
