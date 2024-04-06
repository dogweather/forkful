---
date: 2024-01-27 20:34:34.021255-07:00
description: "\u65B9\u6CD5\uFF1A JavaScript\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\
  \u308B\u3082\u3063\u3068\u3082\u7C21\u5358\u306A\u65B9\u6CD5\u306F\u3001`Math.random()`\u3092\
  \u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u95A2\u6570\u306F\
  \u30010\uFF08\u542B\u3080\uFF09\u304B\u30891\uFF08\u542B\u307E\u306A\u3044\uFF09\
  \u307E\u3067\u306E\u7BC4\u56F2\u306E\u6D6E\u52D5\u5C0F\u6570\u70B9\u3067\u64EC\u4F3C\
  \u4E71\u6570\u3092\u8FD4\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.670543-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u3082\u3063\u3068\
  \u3082\u7C21\u5358\u306A\u65B9\u6CD5\u306F\u3001`Math.random()`\u3092\u4F7F\u7528\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u95A2\u6570\u306F\u30010\uFF08\
  \u542B\u3080\uFF09\u304B\u30891\uFF08\u542B\u307E\u306A\u3044\uFF09\u307E\u3067\u306E\
  \u7BC4\u56F2\u306E\u6D6E\u52D5\u5C0F\u6570\u70B9\u3067\u64EC\u4F3C\u4E71\u6570\u3092\
  \u8FD4\u3057\u307E\u3059."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法：


### 基本的な乱数生成
JavaScriptで乱数を生成するもっとも簡単な方法は、`Math.random()`を使用することです。この関数は、0（含む）から1（含まない）までの範囲の浮動小数点で擬似乱数を返します。

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### 範囲内での乱数生成
しばしば、特定の範囲内のランダムな整数が欲しい場合があります。これは、`Math.random()`の出力をスケーリングして丸めることにより実現できます。

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### 暗号学的に安全な乱数
ランダムさの度合いがより高い（例えば、暗号化操作など）アプリケーションには、`crypto.getRandomValues()`メソッドを使用できます。これは、`Math.random()`によって生成される擬似乱数とは異なり、暗号化ランダムさを提供します。

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## 詳細
歴史的に、JavaScriptでの乱数生成は`Math.random()`関数にのみ依存していました。大半の一般的な使用例には便利ですが、そのアルゴリズムは、メルセンヌ・ツイスターなどの擬似乱数生成器（PRNG）の変種が一般的で、暗号化セキュリティを提供しません。

Web Cryptography APIの導入により、`crypto.getRandomValues()`メソッドが提供され、セキュリティに敏感なアプリケーションに向けて予測がはるかに難しく適した数値を生成する方法が提供されました。この方法は、暗号化操作に適したより頑強な`/dev/random`など、基礎となるオペレーティングシステムのランダムソースにアクセスします。

任務に適した方法を選択することが重要です。`Math.random()`は簡単なゲーム、アニメーション、あるいはランダムの品質が重視されない場合に十分です。しかし、パスワードリセットトークンや暗号化操作などのセキュリティ機能については、その優れたランダム品質のために`crypto.getRandomValues()`がより良い選択肢となります。

特に`Math.random()`は、ほとんどの実装で既知の偏りを持つ数値を生成します。つまり、一部の数値が他よりも発生しやすいということです。この偏りはごく小さく、通常のアプリケーションではほとんど感じられないものの、暗号文脈やオンラインギャンブルのような公正さが重要なアプリケーションでは`Math.random()`の使用を不適格とします。

結論として、JavaScriptの内蔵された乱数生成機能は幅広いニーズをカバーしますが、それぞれの方法の違いと制限を理解することは、適切な適用に不可欠です。
