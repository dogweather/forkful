---
title:                "複素数の扱い方"
date:                  2024-01-26T04:36:57.590884-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数には実部と虚部があり、通常`a + bi`として記述されます。信号処理や電気工学、または現象が平面上で最もよくモデル化されるその他の分野を含む、数学を多用するArduinoプロジェクトに不可欠です。

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
