---
title:                "数値の丸め処理"
date:                  2024-01-26T03:45:25.359702-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
数字の丸めとは、数値を最も近い整数や指定された小数点以下の桁に調整することです。これは値を単純化し、読みやすくするため、または通貨のように特定の制約に合わせるために行われます。

## 方法：
Goの`math`パッケージを使えば、数字の丸めが簡単にできます。`math.Round`、`math.Floor`、`math.Ceil`を使ってみましょう:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // 最も近い整数に丸める
	fmt.Println("Floor:", math.Floor(number)) // 切り捨て
	fmt.Println("Ceil: ", math.Ceil(number))  // 切り上げ
}
```

サンプル出力:
```
Round: 3
Floor: 3
Ceil: 4
```

特定の小数点以下の桁に丸めるには、掛けて丸めて割ります:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("2小数点以下に丸めた:", roundToDecimalPlace(number, 2))
}
```

サンプル出力:
```
2小数点以下に丸めた: 3.14
```

## 奥深く
数字を丸めるのは新しいことではなく、古代の数学にまで遡り、常に単純化を目指していました。Goの`math.Round`は[銀行家の丸め](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even)を使用しており、0.5は最も近い偶数に丸められ、合計に影響を与える可能性のあるバイアスを減らします。

浮動小数点数はその2進表現のために扱いが難しいことがあり、全ての小数を正確に表現できないことがあります。しかし、Goのアプローチは、ほとんどの場合、期待される挙動を維持します。

他にも「半分を切り上げる」「ゼロから遠くに半分を切り上げる」などの丸め方法が存在しますが、すぐに利用可能なのはGoの標準ライブラリです。より複雑なニーズには、サードパーティのライブラリを利用するか、独自の解決策を見つける必要があるかもしれません。

## 参照
- Goの`math`パッケージ：[https://pkg.go.dev/math](https://pkg.go.dev/math)
- 浮動小数点数の算術に関するIEEE 754標準（Goが浮動小数点数を扱う基礎）：[https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- 浮動小数点数についての理解：["What Every Computer Scientist Should Know About Floating-Point Arithmetic"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
