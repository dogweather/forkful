---
date: 2024-01-26 03:47:12.006620-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u306E\u6570\u5024\u306E\u4E38\u3081\
  \u306F\u3001\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u3092\u4F7F\u7528\u3057\u3066\
  \u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u306B\u7C21\
  \u5358\u306A\u8AAC\u660E\u3092\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.750567-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u3067\u306E\u6570\u5024\u306E\u4E38\u3081\u306F\u3001\u3044\u304F\
  \u3064\u304B\u306E\u65B9\u6CD5\u3092\u4F7F\u7528\u3057\u3066\u884C\u3046\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u306B\u7C21\u5358\u306A\u8AAC\u660E\
  \u3092\u3057\u307E\u3059."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## 方法：
TypeScriptでの数値の丸めは、いくつかの方法を使用して行うことができます。ここに簡単な説明をします。

```typescript
// Math.roundは最も近い整数に丸める
console.log(Math.round(1.5)); // 出力: 2

// Math.ceilは最も近い整数まで切り上げる
console.log(Math.ceil(1.1)); // 出力: 2

// Math.floorは最も近い整数まで切り捨てる
console.log(Math.floor(1.8)); // 出力: 1

// toFixedは固定された小数点以下の桁数で丸める
let num = 1.23456;
console.log(num.toFixed(2)); // 出力: "1.23"
// 注：toFixedは文字列を返す！必要に応じてparseFloatを使って戻すこと。
console.log(parseFloat(num.toFixed(2))); // 出力: 1.23
```

## 深掘り
昔、初期のコンピュータでは限られた空間と精度の問題のために数値の丸めが必須でした。今日、浮動小数点演算は二進法で数値が格納される方法のために、風変わりな結果を引き起こすことがあります。丸めの代わりになる方法には、floor、ceil、decimalsを切り捨てるが丸めないtruncが含まれます。

内部の動作に注目する価値があります：`Math.round`は「半分を上へ丸める」（いわゆる「商業的丸め」）に従いますが、`Math.floor`と`Math.ceil`は直接的です。`toFixed`は文字列を返し、特に同じ数値を複数回丸める際のバイアスを減らすために有用な「半分を偶数に丸める」（いわゆる「銀行家の丸め」）を使用するため、予期せぬ結果を引き起こすかもしれません。

## 参照
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE 浮動小数点演算標準 (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
