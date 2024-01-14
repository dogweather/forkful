---
title:                "TypeScript: ランダムな数字を生成する"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
ランダムな数字を生成することに興味を持つ理由は多々あります。たとえば、ゲームやシミュレーションの開発においてランダムな要素が必要な場合、またはセキュリティ目的での乱数生成などに使用されることがあります。プログラマーとしてランダムな数字を生成するスキルは、様々な用途において役立つことができます。

## 生成方法
ランダムな数字を生成するには、TypeScriptの組み込み関数である`Math.random()`を使用します。この関数は、0以上1未満のランダムな小数を返します。具体的なコード例は以下の通りです。

```TypeScript
// 0から10までのランダムな数字を生成する
const randomNum = Math.floor(Math.random() * 11);
console.log(randomNum); // 例: 7
```

エスケープシーケンスを使用することで、より多様なランダムな数字の範囲を指定することも可能です。

```TypeScript
// 1から100までのランダムな数字を生成する
const randomNum = Math.floor(Math.random() * 101);
console.log(randomNum); // 例: 56
```

## 深堀り
ランダムな数字を生成する方法は、数学的にも非常に興味深いトピックです。乱数生成アルゴリズムには、線形合同法やメルセンヌ・ツイスター法などがあります。これらのアルゴリズムは、コンピューターでの乱数生成の基礎となっており、興味がある方は是非調べてみてください。

## See Also
- [TypeScript公式ドキュメント: Math](https://www.typescriptlang.org/docs/handbook/standard-library.html#math)
- [ランダムな数値を生成する方法(MDN)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)