---
title:                "C: ランダムな数字の作成"
simple_title:         "ランダムな数字の作成"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
乱数を生成することの*なぜ*が重要なのでしょうか？乱数はコンピュータープログラミングにおいて非常に重要な役割を果たしています。例えば、ゲームやシミュレーションプログラムでは、乱数を用いることでプレイヤーに対してランダムな挑戦を提供したり、リアルな状況を再現したりすることができます。

## 生成方法
乱数を生成する方法はいくつかありますが、ここではC言語を使用したコーディング例を紹介します。以下のコードは、1から10までのランダムな整数を生成するものです。

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    int num = rand() % 10 + 1;
    printf("生成された乱数は%dです。\n", num);
    return 0;
}
```
このコードを実行すると、以下のような出力結果が得られます。

> 生成された乱数は7です。

これは毎回実行するたびに異なる値が得られることを確認できるはずです。

## 深堀り
乱数を生成する方法には、線形合同法やメルセンヌ・ツイスター法など、さまざまなアルゴリズムがあります。これらのアルゴリズムはそれぞれ特有の特徴があり、異なる用途に合わせて使い分けることができます。また、乱数の周期や偏りなども気にする必要があります。深堀りすると、乱数生成においてさまざまな考慮点があることがわかります。

## また見てね
乱数生成についてさらに学びたい方は、以下のリンクを参考にしてみてください。

- [乱数生成の様々な方法](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E7%94%9F%E6%88%90%E3%81%AE%E6%96%B9%E6%B3%953)
- [C言語における乱数生成の仕組み](http://www.yamame-knife.jp/sample/c/rand.html)
- [乱数生成における周期と偏りの問題](https://doratex.hatenablog.jp/entry/20181005/1538757635)

これらの情報を参考にすることで、より深く乱数生成について理解を深めることができるでしょう。

## さらに見る

- [C言語における乱数](https://github.com/bls1999/RandomNumberGeneration)
- [乱数を使ったゲーム作成チュートリアル](https://www.freecodecamp.org/news/how-to-create-a-simple-html5-canvas-game-ca9eeb5144d8/)