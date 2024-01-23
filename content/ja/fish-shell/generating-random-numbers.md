---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:00.062949-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
ランダム数生成とは予測不可能な数値を作り出すことであり、テストやシミュレーション、ゲームの要素など多岐にわたるプログラミング場面で活用されます。

## How to:
Fish Shellでは、`random` コマンドを使って簡単にランダムな数字を生成できます。ここに例を示します:

```Fish Shell
# 1から10までのランダムな数字を生成
set random_num (random 1 10)
echo $random_num
```

実行例:

```
$ fish
> set random_num (random 1 10)
> echo $random_num
7
```

## Deep Dive
Fish Shellの`random` コマンドは以前は`shuf` コマンドに依存していましたが、現在は内蔵コマンドとして独自に実装されています。これにより、外部ツールに頼らずに使用できるようになりました。Bashや他のシェルスクリプトの `RANDOM` 変数や `openssl rand` 、PythonやJavaScriptの乱数生成機能と比較すると、Fishではコマンド一つで直感的に数値を生成できるのが特徴です。

## See Also
- Fish Shellの公式ドキュメント: https://fishshell.com/docs/current/cmds/random.html
- POSIX シェルスクリプトのランダム数: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
- より高度な乱数生成について: https://en.wikipedia.org/wiki/Random_number_generation
