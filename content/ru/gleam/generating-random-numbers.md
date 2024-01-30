---
title:                "Генерация случайных чисел"
date:                  2024-01-28T23:58:55.446159-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерация случайных чисел"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Генерация случайных чисел в программировании может быть критически важной для создания симуляций, тестирования, криптографии и игр. В Gleam это функция, которая позволяет разработчикам вносить непредсказуемость или симулировать реальные сценарии в их приложениях.

## Как:

Чтобы сгенерировать случайные числа в Gleam, в первую очередь используется библиотека `gleam_random`. Эта библиотека предоставляет функции для генерации случайных целых чисел, чисел с плавающей точкой и других. Сначала убедитесь, что вы добавили `gleam_random` в файл `rebar.config` или `mix.exs` в качестве зависимости.

Давайте рассмотрим некоторые примеры:

### Генерация случайного целого числа

Чтобы получить случайное целое число в заданном диапазоне, можно использовать функцию `int`:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Эта функция сгенерирует случайное целое число от 1 до 10 включительно.

### Генерация случайного числа с плавающей точкой

Чтобы получить случайное число с плавающей точкой, используйте функцию `float`. Это генерирует число с плавающей точкой между 0.0 и 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Пример вывода

Выполнение этих функций может привести к таким результатам, как:

- Для `generate_random_int()`: `5`
- Для `generate_random_float()`: `0.84372`

Помните, что каждое выполнение может привести к различным результатам из-за природы случайности.

## Подробнее

Модуль `gleam_random` реализует генератор псевдослучайных чисел (PRNG), что фактически означает, что числа не совсем случайны, но их трудно предсказать, что имитирует случайность. PRNG работают, начиная с начального значения, известного как семя, и применяя математические операции для генерации последовательности чисел.

Исторически, языки и библиотеки реализовали несколько алгоритмов для PRNG, таких как Mersenne Twister или линейный конгруэнтный генератор (LCG). Выбор алгоритма влияет на качество "случайности", причём некоторые из них более подходят для криптографических приложений, чем другие. Хотя стандартная библиотека Gleam обеспечивает удобство и простоту использования с её модулем `gleam_random`, она может не всегда быть лучшим выбором для случаев, требующих криптографически защищённой случайности. Для криптографических целей разработчикам следует искать библиотеки, специально разработанные для предоставления криптографически защищённых генераторов псевдослучайных чисел (CSPRNG), которые разработаны для противодействия атакам, способным предсказать будущие числа на основе наблюдения за последовательностью сгенерированных чисел.

В заключение, хотя функциональность генерации случайных чисел в Gleam надёжна для общих программных потребностей, приложения с конкретными требованиями безопасности должны рассмотреть специализированные криптографические решения, чтобы обеспечить целостность и безопасность их генерации случайных чисел.