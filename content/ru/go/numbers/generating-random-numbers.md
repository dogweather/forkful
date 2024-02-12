---
title:                "Генерация случайных чисел"
date:                  2024-02-03T17:57:53.541543-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерация случайных чисел"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Генерация случайных чисел в программировании заключается в создании последовательности чисел, предсказать которую разумно точно можно не лучше, чем наугад. Программисты делают это по множеству причин, включая симуляции, игры и приложения безопасности, где непредсказуемость ключевая для функциональности или секретности.

## Как это делается:

В Go случайные числа генерируются с использованием пакета `math/rand` для псевдослучайных чисел или `crypto/rand` для криптографически безопасных псевдослучайных чисел. Давайте рассмотрим оба варианта.

### Использование `math/rand` для Псевдослучайных Чисел

Сначала импортируйте пакет `math/rand` и пакет `time` для инициализации генератора. Инициализация обеспечивает получение различной последовательности чисел при каждом запуске.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Случайное число:", rand.Intn(100)) // Генерирует число от 0 до 99
}
```

Пример вывода: `Случайное число: 42`

### Использование `crypto/rand` для Криптографически Безопасных Псевдослучайных Чисел

Для приложений, чувствительных к безопасности, подходит пакет `crypto/rand`, поскольку он генерирует случайные числа, которые трудно предсказать, делая их подходящими для криптографических операций.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Безопасное случайное число:", n)
}
```

Пример вывода: `Безопасное случайное число: 81`

## Глубокое Погружение

Основное различие между пакетами `math/rand` и `crypto/rand` в Go заключается в их источнике энтропии и предназначении. `math/rand` генерирует псевдослучайные числа на основе начального значения; таким образом, последовательность детерминирована и может быть предсказана, если известно начальное значение. Это подходит для сценариев, где ключевым является высокая производительность, а не абсолютная непредсказуемость, как в симуляциях или играх.

С другой стороны, `crypto/rand` получает случайность из базовой операционной системы, что делает его подходящим для криптографических применений, где непредсказуемость имеет решающее значение. Однако это связано с потерей производительности и сложностью обращения с генерируемыми числами (например, с использованием типа `*big.Int` для целых чисел).

Исторически понятие генерации случайных чисел в компьютерах всегда балансировало на грани истинной "случайности", с ранними системами, сильно зависевшими от детерминированных алгоритмов, имитирующих случайность. С развитием компьютеров эти алгоритмы также развивались, включая более сложные источники энтропии из их окружения.

Несмотря на эти достижения, стремление к совершенной случайности в вычислениях по своей сути парадоксально, учитывая детерминированную природу самих компьютеров. Это почему, для большинства приложений, где предсказуемость была бы вредной, криптографически безопасные псевдослучайные числа из источников вроде `crypto/rand` являются лучшей альтернативой, несмотря на их накладные расходы.

По сути, подход Go с двумя разными пакетами для генерации случайных чисел элегантно адресует компромисс между производительностью и безопасностью, позволяя разработчикам выбирать в зависимости от их конкретных потребностей.