---
title:                "数値の丸め処理"
date:                  2024-01-26T03:47:12.006620-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/rounding-numbers.md"
---

{{< edit_this_page >}}

## なぜ＆どうして？
数値の丸めは、特定の精度に数値を整えることです。プログラマーは、数値の出力を制御するため、可読性や表示目的のため、または浮動小数点の結果を得る操作の後に特定の精度が必要な場合にこれを行います。

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