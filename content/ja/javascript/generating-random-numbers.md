---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:33.360559-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

ランダムな数字を生成するって、まさに文字通りの意味だよ。プログラム内で予測不能な数値が必要なときに使うテクニックさ。ゲーム、セキュリティ、科学的シミュレーションなど、多種多様な場面で使われてるね。

## How to: (方法)

```Javascript
// 基本的な乱数生成
let randomNum = Math.random();
console.log(randomNum); // 0以上1未満の浮動小数点数

// 1から10までの整数をランダムに生成する
let randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt); // 1 から 10 の整数
```

サンプル出力:
```
0.437829823422431
7
```

## Deep Dive (掘り下げ)

乱数生成はコンピューターの歴史とともに進化してきた。`Math.random()`はJavaScriptで最も基本的な乱数生成関数だが、完全にはランダムじゃない。これは擬似乱数生成器で、アルゴリズムに基づいているからね。シード値に依存し、完全なランダムさを求める場合はCrypto APIを使うといい。

別の方法としては、Web Crypto APIの`crypto.getRandomValues()`が挙げられる。このAPIはよりセキュリティーに強い乱数を生成するよ。

例:
```Javascript
let array = new Uint32Array(10);
window.crypto.getRandomValues(array);
console.log(array); // より安全なランダムな整数の配列
```

`Math.random()`はほとんどの用途で十分だが、セキュリティ関連の用途では`crypto.getRandomValues()`の使用が推奨される。

## See Also (関連情報)

- MDN Web Docs - Math.random: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- MDN Web Docs - crypto.getRandomValues(): https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
- Random.org - True Random Number Service: https://www.random.org/
