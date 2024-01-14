---
title:    "Gleam: ランダムな数字の生成"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

#なぜ乱数を生成するのか？

乱数は、プログラミングの世界ではとても便利なツールです。特に、ゲームやシミュレーションなど、ランダム性が必要な場面で活用されます。また、ランダム性を持たせることで、プログラムの予測可能性を下げることができ、セキュリティ上の利点もあります。

## 生成方法

乱数を生成するには、 `random` モジュールを使用します。まずはこのモジュールをインポートしましょう。

```Gleam
import random
```

次に、`random`モジュールの `generate_int`関数を使用して、指定した範囲内の整数を生成することができます。

```Gleam
let random_number = random.generate_int(1, 10) // 1から10までの整数を生成
```

また、浮動小数点数を生成する場合には、`generate_float`関数を使用します。

```Gleam
let random_float = random.generate_float(0.0, 1.0) // 0から1までの浮動小数点数を生成
```

乱数を生成するたびに、同じ結果が返ってくるのも問題です。それを避けるためには、毎回異なるシード値を生成する必要があります。

```Gleam
let seed = random.generate_seed()
random.seed(seed) // シード値をセット
```

## 深堀り

乱数は実際には、疑似乱数と呼ばれるものであり、完全にランダムな値ではありません。乱数生成アルゴリズムには、繰り返し周期があり、同じ数値の列が現れることもあり得ます。

一方、真にランダムな乱数を生成するハードウェアデバイスも存在しますが、プログラミングでは主に疑似乱数が使用されます。

より安全で高品質な、疑似乱数を生成するアルゴリズムを実装することも可能ですが、その分処理負荷が高くなることに注意が必要です。

## さらに詳しく知るには

- [Gleam公式ドキュメント: ランダムモジュール](https://gleam.run/documentation/stdlib/random.html)
- [乱数 - 维基百科](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0)
- [疑似乱数 - 维基百科](https://ja.wikipedia.org/wiki/%E7%96%91%E4%BC%BC%E4%B9%B1%E6%95%B0)

#関連リンク

- [Gleam公式サイト](https://gleam.run/)
- [Gleam日本公式サイト](https://gleam-lang.org/ja/)
- [Gleam GitHubリポジトリ](https://github.com/gleam-lang/gleam)