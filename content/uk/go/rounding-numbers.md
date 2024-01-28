---
title:                "Округлення чисел"
date:                  2024-01-26T03:45:09.985316-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Округлення чисел полягає в коригуванні числа до найближчого цілого або указаного десяткового знаку. Це робиться для спрощення значень, їх більшої зрозумілості або для інтеграції в певні обмеження, наприклад, при роботі з валютами.

## Як:
Пакет `math` в Go - ваш друг для округлення. Використовуйте `math.Round`, `math.Floor` і `math.Ceil` для спрощення:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Округлення до найближчого цілого числа
	fmt.Println("Floor:", math.Floor(number)) // Округлення вниз
	fmt.Println("Ceil: ", math.Ceil(number))  // Округлення вгору
}
```

Приклад виводу:
```
Round: 3
Floor: 3
Ceil: 4
```

Для специфічних десяткових знаків множте, округляйте, потім діліть:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Округлено до 2 десяткових знаків:", roundToDecimalPlace(number, 2))
}
```

Приклад виводу:
```
Округлено до 2 десяткових знаків: 3.14
```

## Поглиблено
Округлення чисел не є новинкою — воно сягає корінням в давню математику, завжди прагнучи до простоти. `math.Round` в Go використовує [округлення банкірів](https://uk.wikipedia.org/wiki/Okruglennya#Round_half_to_even) (англ. bankers' rounding), що означає, що 0.5 округлюється до найближчого парного числа, зменшуючи упередження, яке могло б вплинути на суми.

Числа з плаваючою комою можуть бути хитрими через їх двійкове представлення, яке може не завжди точно відтворювати всі десяткові знаки. Однак, підхід Go, як правило, зберігає очікувану поведінку більшу частину часу.

Існують інші методи округлення, такі як "округлення до найближчого відносно пів" або "округлення від нуля", але стандартна бібліотека Go є тим, що легко доступно. Для більш складних потреб може знадобитися стороння бібліотека або розробка власного рішення.

## Див. також
- Пакет `math` в Go: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- Стандарт IEEE 754 для арифметики з плаваючою комою (основа Go для роботи з плаваючими числами): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Розуміння чисел з плаваючою комою: ["Що кожен комп'ютерний науковець повинен знати про арифметику чисел з плаваючою комою"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
