---
title:                "Створення випадкових чисел"
html_title:           "Gleam: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому

Генерація випадкових чисел - це важлива частина багатьох програм і ігор. Це дозволяє створювати різноманітні, неочікувані інтерактивні елементи, що збагачують користувацький досвід.

## Як це зробити

```Gleam
1 | let random_number = Random.float(0, 100) 
2 | let random_int = Random.int(1, 10)
3 | 
4 | println(random_number)
5 | // Output: 35.97
6 | println(random_int)
7 | // Output: 6 
```

Тут ми використовуємо вбудований модуль Random, який надає методи для генерації випадкових чисел. В першому прикладі, ми використовуємо метод Random.float для генерації випадкового числа з діапазону від 0 до 100. В другому прикладі, ми використовуємо метод Random.int для генерації випадкового цілого числа з діапазону від 1 до 10.

## Глибокий погляд

Під капотом модуля Random використовується алгоритм генерації псевдовипадкових чисел, що базується на принципах статистичної генерації. Це дозволяє робити числа досить випадковими для більшості використань, але не є повністю випадковими, забезпечуючи при цьому швидку та ефективну роботу.

## Див. Також

- [Deno рандомні числа](https://deno.land/std/random)
- [Рандомні числа в Python](https://realpython.com/python-random/)
- [Стаття про Генерацію випадкових чисел за допомогою Math.random() у JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math/random)