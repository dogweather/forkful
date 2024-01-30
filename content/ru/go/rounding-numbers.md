---
title:                "Округление чисел"
date:                  2024-01-29T00:02:09.742003-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округление чисел"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/rounding-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Округление чисел предполагает их корректировку до ближайшего целого или указанного десятичного разряда. Это делается для упрощения значений, улучшения их читаемости или для их вписывания в определенные ограничения, например, при работе с валютами.

## Как это сделать:
Пакет `math` в Go будет вашим помощником для округления. Используйте `math.Round`, `math.Floor` и `math.Ceil` для упрощения:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Округление до ближайшего целого числа
	fmt.Println("Floor:", math.Floor(number)) // Округление вниз
	fmt.Println("Ceil: ", math.Ceil(number))  // Округление вверх
}
```

Пример вывода:
```
Round: 3
Floor: 3
Ceil: 4
```

Для конкретных десятичных разрядов умножайте, округляйте, затем делите:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Округлено до 2 десятичных разрядов:", roundToDecimalPlace(number, 2))
}
```

Пример вывода:
```
Округлено до 2 десятичных разрядов: 3.14
```

## Подробнее
Округление чисел не является новинкой — оно имеет давнюю историю в математике и всегда нацелено на упрощение. `math.Round` в Go использует [округление банкиров](https://ru.wikipedia.org/wiki/Округление#Округление_к_ближайшему_чётному), что означает, что 0.5 округляется до ближайшего четного числа, сокращая смещение, которое могло бы повлиять на суммы.

Числа с плавающей точкой могут быть коварными из-за их двоичного представления, которое может не всегда точно представлять все десятичные числа. Однако подход Go, как правило, сохраняет ожидаемое поведение большую часть времени.

Существуют и другие методы округления, такие как "округление к ближайшему большему" или "округление от нуля", но стандартная библиотека Go является тем, что доступно непосредственно. Для более сложных потребностей вам может потребоваться сторонняя библиотека или разработать собственное решение.

## Смотрите также
- Пакет `math` в Go: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- Стандарт IEEE 754 для арифметики с плавающей точкой (основа Go для работы с плавающими числами): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Понимание арифметики с плавающей точкой: ["Что каждый специалист по информатике должен знать об арифметике с плавающей точкой"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)