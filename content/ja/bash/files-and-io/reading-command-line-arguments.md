---
date: 2024-01-20 17:55:25.694615-07:00
description: "How to: Bash\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\
  \u3092\u6271\u3046\u969B\u306E\u80CC\u666F\u3001\u4EE3\u66FF\u624B\u6BB5\u3001\u5B9F\
  \u88C5\u306E\u8A73\u7D30\u306B\u3064\u3044\u3066\u89E3\u8AAC\u3057\u307E\u3059\u3002\
  \ \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u6271\u3046\u3053\
  \u3068\u306FUnix\u306E\u4F1D\u7D71\u3067\u3042\u308A\u3001\u521D\u671F\u306E\u30B7\
  \u30A7\u30EB\u304B\u3089\u7D99\u627F\u3055\u308C\u3066\u3044\u307E\u3059\u3002`$0,\
  \ $1, ...,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.293507-06:00'
model: gpt-4-1106-preview
summary: "Bash\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u6271\
  \u3046\u969B\u306E\u80CC\u666F\u3001\u4EE3\u66FF\u624B\u6BB5\u3001\u5B9F\u88C5\u306E\
  \u8A73\u7D30\u306B\u3064\u3044\u3066\u89E3\u8AAC\u3057\u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
