---
title:                "乱数を生成する"
html_title:           "TypeScript: 乱数を生成する"
simple_title:         "乱数を生成する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何が必要で、なぜ？

乱数を生成するとは何か、プログラマーがそれをする理由は何かを2-3文で説明します。

プログラマーは、ゲーム、暗号化、テスト、データのランダムなサンプリングなど、さまざまな用途のためにランダムな数値が必要になることがあります。乱数生成は、それらの用途を実現するために重要です。

## 使い方：

```TypeScript
// 0から1までのランダムな小数を生成する例
const randomNumber = Math.random();
console.log(randomNumber); // 例：0.546342

// 範囲を指定してランダムな整数を生成する例
const randomInt = Math.floor(Math.random() * 10) + 1; // 1から10の範囲でランダムな整数を生成
console.log(randomInt); // 例：8
```

## 深堀り：

乱数生成は古くからコンピュータ科学の分野で重要な役割を果たしてきました。過去には様々なアルゴリズムが開発され、現在でも新しいアルゴリズムが研究されています。また、乱数生成の代替手段として、擬似乱数生成器がよく使われます。擬似乱数生成器は、乱数ではなく規則性のある数列を生成するものであり、完全にランダムではありませんが、多くの場合十分なものとして使用されています。

乱数生成は、プログラミング言語の標準ライブラリや、外部ライブラリを使用して実装することができます。言語やライブラリによって、異なるアルゴリズムや細かい設定が提供されている場合がありますので、使用する際にはドキュメンテーションを確認することが重要です。

## 関連情報

- [Math.random() - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [擬似乱数生成器- Wikipedia](https://ja.wikipedia.org/wiki/%E6%93%AC%E4%BC%BC%E4%B9%B1%E6%95%B0%E7%94%9F%E6%88%90%E5%99%A8)