---
date: 2024-01-26 04:36:57.590884-07:00
description: "\u4F7F\u3044\u65B9\uFF1A \u5143\u3005\u3001\u8907\u7D20\u6570\u306F\u61D0\
  \u7591\u7684\u306B\u53D7\u3051\u5165\u308C\u3089\u308C\u307E\u3057\u305F\u304C\u3001\
  \u3055\u307E\u3056\u307E\u306A\u79D1\u5B66\u5206\u91CE\u306E\u4E2D\u5FC3\u3068\u306A\
  \u3063\u3066\u3044\u307E\u3059\u3002\u6B74\u53F2\u7684\u306B\u306F\u3001\u5B9F\u969B\
  \u306E\u89E3\u3092\u6B20\u304F\u591A\u9805\u5F0F\u65B9\u7A0B\u5F0F\u306E\u89E3\u3092\
  \u63D0\u4F9B\u3059\u308B\u3053\u3068\u3067\u8A8D\u3081\u3089\u308C\u307E\u3057\u305F\
  \u3002\u2026"
lastmod: '2024-04-05T21:53:43.304171-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u306F\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u8907\u7D20\
  \u6570\u3092\u542B\u3093\u3067\u3044\u307E\u305B\u3093\u304C\u3001`Complex.h`\u306E\
  \u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6D3B\u7528\u3057\u3066\u6271\
  \u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u3089\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u5185\u90E8\u3067\u306F\u3001\u901A\u5E38\u3001\u5B9F\u90E8\
  \u3068\u865A\u90E8\u30922\u3064\u306E\u30C0\u30D6\u30EB\u3067\u683C\u7D0D\u3057\u3001\
  \u7B97\u8853\u3092\u30B5\u30DD\u30FC\u30C8\u3059\u308B\u305F\u3081\u306B\u6F14\u7B97\
  \u5B50\u3092\u30AA\u30FC\u30D0\u30FC\u30ED\u30FC\u30C9\u3059\u308BComplex\u30AF\u30E9\
  \u30B9\u3092\u5B9A\u7FA9\u3057\u307E\u3059."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 使い方：
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // シリアル通信を開始
  
  Complex myComplex(2, 3); // 複素数 2 + 3i を作成
  Complex anotherComplex(1, 1); // 別の複素数 1 + 1i を作成
  
  // 加算
  Complex result = myComplex + anotherComplex; 
  Serial.print("加算: "); 
  result.print(); // 出力は 3 + 4i
  
  // 乗算
  result = myComplex * anotherComplex; 
  Serial.print("乗算: ");
  result.print(); // 出力は -1 + 5i
}

void loop() {
  // この例では使用されません
}
```
サンプル出力：
```
加算: 3 + 4i
乗算: -1 + 5i
```

## 深く掘り下げる
元々、複素数は懐疑的に受け入れられましたが、さまざまな科学分野の中心となっています。歴史的には、実際の解を欠く多項式方程式の解を提供することで認められました。

Arduinoは標準ライブラリに複素数を含んでいませんが、`Complex.h`のようなライブラリを活用して扱うことができます。これらのライブラリ内部では、通常、実部と虚部を2つのダブルで格納し、算術をサポートするために演算子をオーバーロードするComplexクラスを定義します。

代替案として、本質的に複素数の算術を必要としないアプリケーションでは、他の数学戦略やライブラリの使用を検討してください。ただし、複素数の代わりに浮動小数点を使用すると、一部の問題が過度に単純化される可能性があることを覚えておいてください。

## 参照
- Rob Tillaartによる[Complex.h](https://github.com/RobTillaart/Complex)ライブラリ。
- 複素数の背後にある[数学に関する深い探究](https://mathworld.wolfram.com/ComplexNumber.html)。
