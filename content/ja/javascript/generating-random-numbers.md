---
title:                "Javascript: ランダムな数を生成する"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜランダムな数字を生成するのか

ランダムな数字を生成することは、プログラミングの世界では非常によく使われるテクニックです。例えば、ランダムな数字を使用してゲームを作ったり、ランダムなテストデータを生成したりすることができます。さまざまな用途で活用される、便利な機能です。

## 生成方法

ランダムな数字をJavascriptで生成するには、Mathオブジェクトの`random()`メソッドを使用します。下記のようにコードを記述することで、0から1の間のランダムな小数点数を得ることができます。

```Javascript
Math.random()
```

もし、1から10までのランダムな整数を生成したい場合は、`Math.floor()`メソッドを使い、以下のようにコードを記述します。

```Javascript
Math.floor(Math.random() * 10) + 1
```

ここで、`Math.floor()`メソッドは小数点以下を切り捨てるために使用し、`Math.random()`メソッドに10を掛けることで、0から9までのランダムな小数点数を得ます。最後に1を足すことで、1から10までのランダムな整数を得ることができます。

## 詳しく学ぶ

ランダムな数字を生成する方法は以上ですが、もっと詳しく学びたい方には、乱数生成アルゴリズムや擬似乱数生成器などの概念を学ぶことをお勧めします。また、乱数がどのようにコンピュータによって生成されるか、乱数の使用にあたっての注意点なども学ぶことができます。

## さらに見る

- [Mathオブジェクト - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [乱数生成アルゴリズム - Wikipedia](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E7%94%9F%E6%88%90%E3%82%A2%E3%83%AB%E3%82%B4%E3%83%AA%E3%82%BA%E3%83%A0)
- [乱数生成器の仕組みと擬似乱数 - Qiita](https://qiita.com/kotakato/items/ccae16902c075d53b5d1)