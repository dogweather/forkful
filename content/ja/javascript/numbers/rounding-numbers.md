---
date: 2024-01-26 03:45:29.799429-07:00
description: "\u6570\u5024\u306E\u3042\u308B\u70B9\u4EE5\u964D\u306E\u30CE\u30A4\u30BA\
  \u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3092\u4E38\u3081\u3068\u3044\u3044\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u7CBE\u5EA6\u3092\u5236\u5FA1\
  \u3057\u305F\u308A\u3001\u30E1\u30E2\u30EA\u3092\u7BA1\u7406\u3057\u305F\u308A\u3001\
  \u30E6\u30FC\u30B6\u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\u30FC\u306A\u51FA\u529B\u3092\
  \u4F5C\u6210\u3059\u308B\u305F\u3081\u306B\u4E38\u3081\u3092\u884C\u3044\u307E\u3059\
  \u3002\u4F8B\u3048\u3070\u30012.998\u3092\u304D\u308C\u3044\u306A3\u306B\u5909\u3048\
  \u308B\u3088\u3046\u306A\u5834\u5408\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.668883-06:00'
model: gpt-4-0125-preview
summary: "\u6570\u5024\u306E\u3042\u308B\u70B9\u4EE5\u964D\u306E\u30CE\u30A4\u30BA\
  \u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3092\u4E38\u3081\u3068\u3044\u3044\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u7CBE\u5EA6\u3092\u5236\u5FA1\
  \u3057\u305F\u308A\u3001\u30E1\u30E2\u30EA\u3092\u7BA1\u7406\u3057\u305F\u308A\u3001\
  \u30E6\u30FC\u30B6\u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\u30FC\u306A\u51FA\u529B\u3092\
  \u4F5C\u6210\u3059\u308B\u305F\u3081\u306B\u4E38\u3081\u3092\u884C\u3044\u307E\u3059\
  \u3002\u4F8B\u3048\u3070\u30012.998\u3092\u304D\u308C\u3044\u306A3\u306B\u5909\u3048\
  \u308B\u3088\u3046\u306A\u5834\u5408\u3067\u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？
数値のある点以降のノイズを取り除くことを丸めといいます。プログラマーは精度を制御したり、メモリを管理したり、ユーザーフレンドリーな出力を作成するために丸めを行います。例えば、2.998をきれいな3に変えるような場合です。

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
