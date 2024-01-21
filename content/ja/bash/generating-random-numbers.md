---
title:                "ランダム数の生成"
date:                  2024-01-20T17:48:25.879330-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダムな数値を生成するって、まぁどういうことかと言うとコンピュータで偶然の数を作ることです。プログラマーはなぜこれをやるのか？テストデータを作ったり、セキュリティー関連の仕事をしたり、単純にゲームで運を引き入れたりするためですね。

## How to: (やり方)
```Bash
# 基本的なランダムな数生成
echo $((RANDOM))

# 1から100までの範囲でランダムな数を生成
echo $((1 + RANDOM % 100))

# サンプル出力
$ 29321
$ 57
```

## Deep Dive (深堀り)
ランダム数生成の話は長い歴史があって、コンピューターの初期から重要な役割を果たしてきました。Bashでは`$RANDOM`って環境変数が用意されてて、それが0から32767までの範囲の整数をランダムに返してくれるんです。ただ、真のランダムじゃなくて擬似ランダム数字なんですよ。代わりにあるのが、`/dev/random`や`/dev/urandom`を利用する方法です。これらはシステムのエントロピー（乱雑さ）を使って、もう少し予測不可能な数を提供します。

```Bash
# /dev/urandomを使用してランダムなデータを生成し、10進数に変換
od -vAn -N4 -tu4 < /dev/urandom

# サンプル出力
$ 4139684356
```

## See Also (関連情報)
- Bashのマニュアル: https://www.gnu.org/software/bash/manual/
- `$RANDOM`についての詳細: https://tldp.org/LDP/abs/html/randomvar.html
- `/dev/random`と`/dev/urandom`に関する詳しい議論: https://www.2uo.de/myths-about-urandom/