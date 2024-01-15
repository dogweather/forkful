---
title:                "「ランダムな数字の生成」"
html_title:           "Fish Shell: 「ランダムな数字の生成」"
simple_title:         "「ランダムな数字の生成」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

Fish Shellでのランダムな数字の生成方法

## なぜ 

ランダムな数字を生成することで、あなたのプログラムをより面白くしたり、ランダムにデータを処理する必要がある場合に役立ちます。

## 作り方 

```Fish Shell
set -l random (rand 1 10)
echo $random
```

このコードは、1から10までのランダムな整数を生成し、それを変数に格納しています。次に、`echo`コマンドを使用して、生成されたランダムな数字を表示します。

```Fish Shell
set -l random (math rand 10.0)
echo $random
```

こちらのコードでは、小数点を含むランダムな数字を生成しています。`math rand`コマンドを使用することで、指定した範囲で小数点を含むランダムな数字を生成することができます。

## 深く掘り下げる 

ランダムな数字を生成する方法には、さまざまなアルゴリズムがあります。Fish Shellでは、`rand`と`math rand`コマンドを使用することで、簡単にランダムな数字を生成することができます。しかし、より高度なランダム性を求める場合は、より複雑なアルゴリズムが必要になる可能性があります。また、生成されたランダムな数字の品質を評価する重要性もあります。

## 関連情報 

- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [Mathコマンドの詳細](https://fishshell.com/docs/current/cmds/math.html)
- [Random Number Generation - Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation)