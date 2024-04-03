---
date: 2024-01-27 20:34:34.021255-07:00
description: "JavaScript\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u6280\u8853\
  \u306F\u3001\u30E9\u30F3\u30C0\u30E0\u306A\u6575\u306E\u884C\u52D5\u304C\u5FC5\u8981\
  \u306A\u30B2\u30FC\u30E0\u304B\u3089\u3001\u6697\u53F7\u7684\u306A\u30E9\u30F3\u30C0\
  \u30E0\u3055\u3092\u5FC5\u8981\u3068\u3059\u308B\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\
  \u30A2\u30EB\u30B4\u30EA\u30BA\u30E0\u307E\u3067\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u306E\u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u3092\u4F5C\u308A\u51FA\
  \u3059\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u3053\u306E\u80FD\
  \u529B\u306F\u3001\u30C0\u30A4\u30CA\u30DF\u30C3\u30AF\u306A\u30E6\u30FC\u30B6\u30FC\
  \u30A8\u30AF\u30B9\u30DA\u30EA\u30A8\u30F3\u30B9\u3068\u5B89\u5168\u306A\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u958B\u767A\u306B\u3068\u3063\u3066\u91CD\
  \u8981\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.670543-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u6280\u8853\u306F\
  \u3001\u30E9\u30F3\u30C0\u30E0\u306A\u6575\u306E\u884C\u52D5\u304C\u5FC5\u8981\u306A\
  \u30B2\u30FC\u30E0\u304B\u3089\u3001\u6697\u53F7\u7684\u306A\u30E9\u30F3\u30C0\u30E0\
  \u3055\u3092\u5FC5\u8981\u3068\u3059\u308B\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u30A2\
  \u30EB\u30B4\u30EA\u30BA\u30E0\u307E\u3067\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u306E\u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u3092\u4F5C\u308A\u51FA\u3059\
  \u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u3053\u306E\u80FD\u529B\
  \u306F\u3001\u30C0\u30A4\u30CA\u30DF\u30C3\u30AF\u306A\u30E6\u30FC\u30B6\u30FC\u30A8\
  \u30AF\u30B9\u30DA\u30EA\u30A8\u30F3\u30B9\u3068\u5B89\u5168\u306A\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u958B\u767A\u306B\u3068\u3063\u3066\u91CD\u8981\
  \u3067\u3059\u3002."
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
