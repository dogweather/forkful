---
title:                "Javascript: ランダムな数値を生成する"
simple_title:         "ランダムな数値を生成する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜランダムな数値を生成するのか

プログラミングにおいて、ランダムな数値を生成することは非常に重要です。例えば、ゲームのキャラクターの能力やアイテムの出現、ランダムなパスワードの作成など、さまざまな用途で使用されます。この記事では、JavaScriptを使ってランダムな数値を生成する方法を紹介します。

## 方法

JavaScriptでは、Mathオブジェクトのメソッドを使用することでランダムな数値を生成することができます。まずは、Math.random()メソッドを使用して0以上1未満のランダムな数値を生成してみましょう。

```javascript
const randomNum = Math.random();
console.log(randomNum); // 0以上1未満のランダムな数値が出力される
```

さらに、Math.floor()メソッドを併用することで、整数の範囲でランダムな数値を生成することもできます。

```javascript
const randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum); // 1以上10未満のランダムな整数が出力される
```

また、特定の範囲でランダムな数値を生成する場合は、Math.random()とMath.floor()を少し組み合わせる必要があります。例えば、1から100までのランダムな整数を生成したい場合は、以下のようにします。

```javascript
const randomNum = Math.floor(Math.random() * 100) + 1;
console.log(randomNum); // 1以上100未満のランダムな整数が出力される
```

## 深堀り

実際にJavaScript内でランダムな数値を生成する場合、Math.random()メソッドを使用するのは一般的です。これは、単純に乱数を生成するために使用されるからです。しかし、実際には完全にランダムな数値を生成することはできません。なぜなら、Math.random()メソッドは計算方法に基づいてランダムな数値を生成するため、実際には予測可能なパターンで数値が出力されてしまうからです。

このような問題を解決する方法として、「疑似乱数生成アルゴリズム」があります。これは、さまざまな入力値を使用して乱数を生成することで、よりランダムな数値を得ることができる方法です。しかし、これも完全にランダムな数値を生成するわけではありません。

ランダムな数値を生成する方法については、数学的およびコンピュータサイエンスの分野でさまざまな研究が行われており、さらに多くの方法が提案されています。しかし、現状のところ、以上の方法が最も一般的であり、多くの場合十分なランダム性を持つことができます。

## また見る

- [JavaScriptでの乱数生成の方法](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [疑似乱数生成アルゴリズムについての説明](https://computer.howstuffworks.com/question6971.htm)
- [乱数生成についての数学的アプローチ](https://www.geeksforgeeks.org/generating-random-number-uniformly-distributed-in-a-given-range/)