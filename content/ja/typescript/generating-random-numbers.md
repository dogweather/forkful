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

## なぜ
 ランダムな数字を生成することに参加する理由を最大2文で説明します。

ランダムな数字を生成することが重要な理由の1つは、シミュレーションやゲームなどのアプリケーションで必要なランダムな要素を生成することです。ランダムな数字を利用することで、アプリケーションのリアリティや面白さを向上させることができます。

## 作り方
以下は、TypeScriptを使用してランダムな数字を生成する方法のコーディング例と出力例です。

```TypeScript
// 0から10までのランダムな整数を生成する関数
function generateRandomNumber() {
    return Math.floor(Math.random() * 11);
}

console.log(generateRandomNumber()); // 例：5

// ランダムな数字の配列を生成する関数
function generateRandomArray(length: number) {
    let array = [];
    for (let i = 0; i < length; i++) {
        array.push(Math.floor(Math.random() * 101)); // 0から100までのランダムな整数を追加
    }
    return array;
}

console.log(generateRandomArray(5)); // 例：[10, 76, 42, 87, 34]
```

## 深堀り
ランダムな数字を生成するためには、乱数ジェネレーターと呼ばれるアルゴリズムを使用します。乱数ジェネレーターは、いくつかの数学的な計算を組み合わせて、擬似乱数列を生成します。この擬似乱数列は、乱数のように見えますが、実際には予測可能です。

JavaScriptでMath.random()を使用すると、0から1までの間の小数を返します。これを利用して、必要な範囲のランダムな整数を生成することができます。また、乱数ジェネレーターには独自のアルゴリズムを使用することで、よりランダムな数字を生成することができます。

## 早見表
- ランダムな数字を生成することで、アプリケーションのリアリティや面白さを向上させることができる。
- 乱数ジェネレーターを使用して、擬似乱数列を生成することができる。
- Math.random()を使用することで、0から1までの間のランダムな小数を取得できる。

## 参考リンク
- [TypeScript公式サイト](https://www.typescriptlang.org/)
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Wikipedia - 乱数ジェネレーター](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E3%82%B8%E3%82%A7%E3%83%8D%E3%83%AC%E3%83%BC%E3%82%BF#:~:text=%E4%B9%B1%E6%95%B0%E3%82%B8%E3%82%A7%E3%83%8D%E3%83%AC%E3%83%BC%E3%82%BF%E3%81%AF%E3%80%81%E5%AE%9F%E9%9A%9B,1%E3%81%9E%E3%82%8C%E3%81%82%E3%82%8B%E3%82%82%E3%81%AE%E3%81%8B%E3%82%89%E4%B8%80%E9%83%A8%E5%88%86%E7%9