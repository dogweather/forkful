---
title:    "Go: Створення випадкових чисел"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому
У го повно корисних функцій для генерування випадкових чисел, від простих чисел до складніших структур даних. Це корисно для різних завдань, таких як шифрування, симуляції та тестування програм.

## Як
```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Встановлення "seed" для генерування випадкових чисел
    rand.Seed(time.Now().UnixNano())

    // Генерування випадкового цілого числа від 0 до 100
    fmt.Println(rand.Intn(101))

    // Генерування випадкового числа типу float64 від 0 до 1
    fmt.Println(rand.Float64())

    // Генерування випадкового числа типу float64 від -5 до 5
    fmt.Println(rand.Float64()*10 - 5)
}
```
Вихідний код:
```Go
78 
0.9124365981569181 
-0.4486921809272483
```

## Занурення в деталі
Го використовує алгоритм Мерсенна для генерування випадкових чисел, який базується на лінійній рухомості кімнаток. Це забезпечує швидке та ефективне генерування великих обсягів випадкових чисел.

## Дивись також
- [Документація Go про генерування випадкових чисел](https://golang.org/pkg/math/rand/)
- [Стаття на Medium про використання випадкових чисел в Go](https://medium.com/rungo/the-math-behind-random-number-generation-in-go-294c3e4412d0)
- [Приклади використання генератора випадкових чисел у реальному коді на GitHub](https://github.com/golang/go/search?q=%22math%2Frand%22&unscoped_q=%22math%2Frand%22&type=Code)