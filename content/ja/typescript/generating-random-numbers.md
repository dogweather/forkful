---
title:                "ランダム数の生成"
date:                  2024-01-20T17:50:25.308591-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
乱数を生成することは、予測不可能な数字を作り出すプロセスです。プログラマーは、ゲーム、セキュリティシステム、またはデータ解析のために、さまざまな用途でこれを行います。

## How to: (方法)
TypeScriptで乱数を生成する方法を見ていきましょう。シンプルな数値から、範囲指定された数までの例を紹介します。

```TypeScript
// 0から1未満の乱数を生成
const randomDecimal: number = Math.random();
console.log(randomDecimal);

// 整数乱数を生成: 1から10
const randomInt: number = Math.floor(Math.random() * 10) + 1;
console.log(randomInt);
```

出力例：
```
0.4350287406415234
7
```

## Deep Dive (深堀り)
乱数生成はコンピュータサイエンスで長い歴史があります。完全にランダムな値は計算機では生成できないため、使用されるのは擬似乱数です。`Math.random()` はJavaScript（そしてTypeScript）において内蔵された擬似乱数生成器(PRNG)で、十分にランダムな値を提供するためにアルゴリズムを使います。

代替案として、暗号学的に安全な乱数が必要な場合は、Web Crypto APIの`crypto.getRandomValues()`を使用することができます。この方法は、より予測不可能な値を生成しますが、ブラウザ限定の解決策です。

実装の詳細では、乱数はゲーム内イベント、セッションIDの生成、乱数サンプリングなどに利用されます。正しいツールを選ぶことは、求めているランダム性のレベルとセキュリティの要求によります。

## See Also (関連情報)
- TypeScript公式ドキュメント: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- MDN Web DocsのMath.random(): [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- Web Crypto APIの使い方: [https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)