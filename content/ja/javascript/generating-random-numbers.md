---
title:                "ランダムな数字の生成"
html_title:           "Javascript: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Random Number Generator: What & Why?

ランダムな数字を生成するとは何か？それをする理由はなにか？

ランダムな数字を生成することは、プログラマーがコンピューター上で偶発的な値を作成する方法です。これは、乱数を生成する方法として非常に重要です。プログラマーは、ランダムな要素を必要とするさまざまなアプリケーションやゲームの開発において、ランダムな数字を使用することができます。

## 方法：

```Javascript
// 0から1までのランダムな小数を生成します
Math.random(); // 出力例: 0.8799373467080414

// 指定した範囲内のランダムな整数を生成します
Math.floor(Math.random() * 10) + 1; // 出力例: 7

// 指定した範囲内のランダムな小数を生成します
Math.random() * (max - min) + min; // 出力例: 4.205834635881799
```

## 深く掘り下げる：

ランダムな数字を生成する方法はコンピューターサイエンスにおいて非常に重要です。この概念は、ランダムな数値を生成するための先駆的な手法やアルゴリズムを開発することで発展してきました。代替として、コンピューターシミュレーションや暗号学において、さまざまなランダム性を要求するアプリケーションがあります。ランダムな数字の生成は、乱数テーブルや電子デバイスを使用する物理的な方法ではなく、コンピューター上で算術的な方法を使用しています。

## 関連リンク：

- [MDNのMath.random()ドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [プログラミング用語集：乱数](https://wa3.i-3-i.info/word16202.html)
- [乱数生成器の解説](https://qiita.com/17ec084t/items/dc0089168d5a717b5c76)