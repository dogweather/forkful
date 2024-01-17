---
title:                "ランダムな数を生成する"
html_title:           "Gleam: ランダムな数を生成する"
simple_title:         "ランダムな数を生成する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ランダムな数字を生成するのは何か？

ランダムな数字を生成するとは、プログラマーがコンピューターにランダムな数字を生成させることを指します。これは、ゲームや暗号化など、さまざまなアプリケーションで使用される重要な機能です。

なぜプログラマーがランダムな数字を生成するのかというと、これはプログラムの予測可能性を減らし、セキュリティを向上させるための手段だからです。また、ランダムな数字を生成することで、バランスのとれたランダムな選択をすることができます。

## 方法：

Gleamでは、ランダムな数字を生成するために```random.int(min, max)```関数を使用します。これは、最小値と最大値の範囲内でランダムな整数を生成するものです。

例えば、次のコードを実行すると、1から10の間のランダムな整数が生成されます。

```Gleam
let random_num = random.int(1, 10)
```

出力例：

```Gleam
7
```

## 詳細：

ランダムな数字を生成するためには、偏りのない乱数生成アルゴリズムが使用されます。これは、複数の要因を混ぜ合わせることで実現されます。

Gleam以外のアルゴリズムとしては、線形合同法やメルセンヌ・ツイスター法などがあります。これらは、簡単に実装できる反面、偏りが生じる可能性があります。

ランダムな数字を生成するのには、パソコンが使用する物理的なプロセスも利用されます。これは、ハードウェアに搭載されているノイズジェネレーターを使用する方法や、マウスの移動やキーの操作などのユーザーの入力を使用する方法などがあります。

## 関連リンク：

- Gleam公式ドキュメント： https://gleam.run/
- ランダムな数字生成の歴史： https://en.wikipedia.org/wiki/Random_number_generation
- 線形合同法：https://en.wikipedia.org/wiki/Linear_congruential_generator
- メルセンヌ・ツイスター法：https://en.wikipedia.org/wiki/Mersenne_Twister