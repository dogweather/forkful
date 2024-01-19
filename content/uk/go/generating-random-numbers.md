---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і Для чого?

Генерація випадкових чисел - це процес створення чисел, які неможливо передбачити краще , ніж випадковим вибором. Програмісти це роблять, щоб організувати непередбачувані події, випадковий вибір з декількох опцій або розміщення даних шляхом шифрування.

## Як це зробити:

```Go
package main

import (
   "fmt"
   "math/rand"
   "time"
)

func main() {
   rand.Seed(time.Now().UnixNano())
   randomNumber := rand.Intn(100)
   fmt.Println(randomNumber)
}
```

При виконанні цього скрипту ви побачите випадкове число від 0 до 99.

## Занурення:

В історичному контексті, генерація випадкових чисел була ключовим елементом комп'ютерних наук, починаючи з комп'ютерів часів Другої світової війни, які використовували це для шифрування.

Існують альтернативні способи генерації випадкових чисел, наприклад за допомогою функції `Crypto/rand` для сильнішого варіанту випадкових чисел, але це може бути повільніше й складніше в імплементації.

Щодо деталів реалізації, `rand.Seed(time.Now().UnixNano())` встановлює "зерно" для генератора випадкових чисел, для того щоб генерувати різні значення при кожному запуску.

## Дивитись також:

1. [Random Numbers in Go](https://gobyexample.com/random-numbers)
2. [Math/Rand package](https://golang.org/pkg/math/rand/)
3. [Crypto package](https://golang.org/pkg/crypto/rand/)