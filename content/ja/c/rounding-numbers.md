---
title:                "数値の丸め処理"
date:                  2024-01-26T03:44:42.372970-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
数値を丸めるというのは、ある特定の点を越えた桁を切り捨て、任意で最後に保持された桁を調整することです。プログラマーが丸めを行うのは、正確な値が必要でない場合に精度を減少させるため、浮動小数点のエラーを管理するため、またはユーザーフレンドリーな表示のために数値を準備するためです。

## 方法
Cでは、通常 `floor()`、`ceil()`、または `round()` 関数を使用します。早速見てみましょう：

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```

特定の位で丸めるようなより細かい制御が必要な場合は、乗算して丸めてから除算します：

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Rounded to 2 decimal places: %.2f\n", num_rounded); // 小数点以下2位に丸めた: 3.14
```

## 深掘り
昔、数値の丸めは手作業でのプロセスを意味しました—ペンと紙だけでは重労働でした。コンピューティングによって、これを自動化しましたが、浮動小数点算術は、その二進性の特性のために、正確に表現できない数値があることで微妙な問題をもたらしました。

標準の丸めに代わる方法には、余分な桁を単純に落とす切り捨てや、ちょうど2つの値の間にある場合に最も近い偶数に丸めるバンカーズラウンディングが含まれます。これは、繰り返し計算におけるバイアスを減少させます。

任意の精度の数値を丸めたり、無限大、シグナリングNaN、または非正規値などの特殊なケースを扱う必要がある場合、実装は難しくなります。C標準ライブラリ関数は基本的なことを処理しますが、カスタムの方法で小数を丸める必要がある場合は、`math.h`よりもさらに多くのものが必要になります。

## 参照
- [`<math.h>`ドキュメント](https://en.cppreference.com/w/c/numeric/math)
- [浮動小数点算術](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [浮動小数点計算を検証する罠](https://dl.acm.org/doi/10.1145/1186736.1186737)