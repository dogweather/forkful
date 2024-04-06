---
date: 2024-01-26 03:45:29.799429-07:00
description: "\u65B9\u6CD5\uFF1A JavaScript\u3067\u6570\u5024\u3092\u4E38\u3081\u308B\
  \u65B9\u6CD5\u306F\u3001`Math.round()`\u3001`Math.ceil()`\u3001`Math.floor()`\u3092\
  \u4F7F\u3044\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.158146-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A JavaScript\u3067\u6570\u5024\u3092\u4E38\u3081\u308B\u65B9\
  \u6CD5\u306F\u3001`Math.round()`\u3001`Math.ceil()`\u3001`Math.floor()`\u3092\u4F7F\
  \u3044\u307E\u3059\uFF1A."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## 方法：
JavaScriptで数値を丸める方法は、`Math.round()`、`Math.ceil()`、`Math.floor()`を使います：

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (.567が.5より大きいため)

console.log(roundedDown); // 出力：2
console.log(roundedUp);   // 出力：3
console.log(rounded);     // 出力：3
```

特定の小数点以下の桁数に修正するには、`toFixed()`を使います：

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (文字列を返す)

console.log(twoDecimals); // 出力："2.57"
```

文字列を数値に戻すには、単項プラスまたは`Number()`を使います：

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // 出力：2.57
```

## 深掘り
数値の丸めは新しいことではありません。それは数字が存在する限り古い技術です。JavaScriptでは、`Math.round()`は「half up」を丸める方法を使います：分数部が0.5の場合、最も近い偶数に丸められます。

より細かい制御が必要な場合、`toFixed()`が適しているかもしれませんが、文字列を返すことを覚えておいてください。数値に戻すのは追加のステップかもしれませんが、数値型で作業を続けることを保証します。

代替手段？`lodash`のようなライブラリは、より繊細な制御のために`_.round(number, [precision=0])`を提供します。または、新しい`Intl.NumberFormat`は丸めだけでない高精度のフォーマットを提供します。

精度について言えば、JavaScriptの浮動小数点の奇妙な点に注意してください。`0.1 + 0.2`は厳密には`0.3`と等しくないのは、数値がどのように格納されているかによるものです。時には、このような浮動小数点のエラーを修正するために丸めが必要になります。

## 参照
- MozillaのMathドキュメント：[MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- `Intl.NumberFormat`による金融丸め：[ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash`の丸め：[Lodash Docs](https://lodash.com/docs/4.17.15#round)
