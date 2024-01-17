---
title:                "Видобування підрядків"
html_title:           "Haskell: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і чому?

Вилучення підстрічок - це процес вибору частини ланцюжка символів зі стрічки за допомогою певної логіки. Програмісти часто використовують цю техніку для отримання певних даних зі стрічок, таких як імена файлів, електронні адреси чи номери телефонів.

## Як це зробити:

Ви можете використовувати функцію `take` та `drop` для вилучення підстрічки зі стрічки в Haskell. Наприклад:

```Haskell 
take 5 "Hello, world!" -- результат: "Hello"
drop 5 "Hello, world!" -- результат: ", world!"
```

Ви також можете використовувати шаблони для вибору підстрічки, використовуючи операцію `$`:

```Haskell
(\x -> take 5 x) $ "Hello, world!" -- результат: "Hello"
```

## Глибоке занурення:

Вилучення підстрічок є важливою частиною багатьох програм і мов програмування, але спосіб, яким вони реалізовані, може різнитися. Наприклад, мова Python має методи `startswith` та `endswith`, які дозволяють здійснювати подібні операції. У Haskell, ви можете використовувати функцію `slice` для вибору підстрічки. Основний метод вибору підстрічки - використання числових індексів, але є й інші способи реалізації.

## Дивись також:

- Документація по функціям `take` та `drop` в Haskell: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:take
- Офіційна документація з використання мови Python: https://docs.python.org/3/library/stdtypes.html#str.startswith
- Стаття про використання шаблонів у Haskell: https://wiki.haskell.org/Pattern_matching