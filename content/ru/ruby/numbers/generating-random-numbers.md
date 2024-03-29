---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:58.160144-07:00
description: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\u043B\u0443\
  \u0447\u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B \u0432 Ruby\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0441\u043E\u0437\u0434\u0430\
  \u043D\u0438\u0435 \u0447\u0438\u0441\u0435\u043B, \u043A\u043E\u0442\u043E\u0440\
  \u044B\u0435 \u043B\u043E\u0433\u0438\u0447\u0435\u0441\u043A\u0438 \u043D\u0435\
  \ \u043C\u043E\u0433\u0443\u0442 \u0431\u044B\u0442\u044C \u043F\u0440\u0435\u0434\
  \u0441\u043A\u0430\u0437\u0430\u043D\u044B, \u0447\u0442\u043E \u043D\u0435\u043E\
  \u0431\u0445\u043E\u0434\u0438\u043C\u043E \u0434\u043B\u044F \u0442\u0430\u043A\
  \u0438\u0445 \u0441\u0446\u0435\u043D\u0430\u0440\u0438\u0435\u0432, \u043A\u0430\
  \u043A \u0441\u0438\u043C\u0443\u043B\u044F\u0446\u0438\u0438,\u2026"
lastmod: '2024-03-13T22:44:45.986452-06:00'
model: gpt-4-0125-preview
summary: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\u043B\u0443\
  \u0447\u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B \u0432 Ruby\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0441\u043E\u0437\u0434\u0430\
  \u043D\u0438\u0435 \u0447\u0438\u0441\u0435\u043B, \u043A\u043E\u0442\u043E\u0440\
  \u044B\u0435 \u043B\u043E\u0433\u0438\u0447\u0435\u0441\u043A\u0438 \u043D\u0435\
  \ \u043C\u043E\u0433\u0443\u0442 \u0431\u044B\u0442\u044C \u043F\u0440\u0435\u0434\
  \u0441\u043A\u0430\u0437\u0430\u043D\u044B, \u0447\u0442\u043E \u043D\u0435\u043E\
  \u0431\u0445\u043E\u0434\u0438\u043C\u043E \u0434\u043B\u044F \u0442\u0430\u043A\
  \u0438\u0445 \u0441\u0446\u0435\u043D\u0430\u0440\u0438\u0435\u0432, \u043A\u0430\
  \u043A \u0441\u0438\u043C\u0443\u043B\u044F\u0446\u0438\u0438,\u2026"
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\u043B\u0443\u0447\
  \u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B"
---

{{< edit_this_page >}}

## Что и Почему?

Генерация случайных чисел в Ruby включает создание чисел, которые логически не могут быть предсказаны, что необходимо для таких сценариев, как симуляции, криптография и игры. Программисты используют случайность для добавления непредсказуемости или имитации переменчивости реальной жизни в своих приложениях.

## Как:

Ruby предоставляет несколько методов для генерации случайных чисел, в первую очередь через класс `Random`.

### Базовое Случайное Число

Чтобы сгенерировать базовое случайное число:

```Ruby
puts rand(10) # Генерирует случайное число между 0 и 9
```

### Случайное Число в Диапазоне

Для случайного числа в определенном диапазоне:

```Ruby
puts rand(1..10) # Генерирует случайное число между 1 и 10
```

### Использование Класса Random

Чтобы создать повторяемую последовательность случайных чисел, вы можете использовать класс `Random` с сидом.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Генерирует предсказуемое "случайное" число
```

### Генерация Случайного Элемента Массива

Выбор случайного элемента из массива:

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # Случайно выбирает элемент из массива
```

### Пример Вывода:

Каждый фрагмент кода выше при выполнении будет производить разные результаты из-за их случайной природы. Например, `rand(10)` может вывести `7`, в то время как `colors.sample` может вывести `"green"`.

## Глубокое Погружение

Концепция генерации случайных чисел в информатике является парадоксальной, потому что компьютеры следуют детерминированным инструкциям. Ранние методы сильно зависели от внешнего ввода для достижения непредсказуемости. Случайность в Ruby основана на алгоритме Mersenne Twister, псевдослучайном числовом генераторе, известном своим огромным периодом и равномерным распределением, что делает его очень подходящим для приложений, требующих высококачественной случайности.

Хотя встроенные методы Ruby хорошо удовлетворяют большинство потребностей, они могут не подойти для всех криптографических целей, поскольку предсказуемость псевдослучайных чисел может быть уязвимостью. Для криптографической безопасности разработчики Ruby могут исследовать библиотеки вроде `OpenSSL::Random`, которые предназначены для производства криптографически безопасных случайных чисел, обеспечивая более высокую непредсказуемость для чувствительных приложений.
