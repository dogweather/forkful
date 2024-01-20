---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Інтерполяція рядка - це техніка вставки змінних або виразів всередину рядка. Це зручно для програмістів, коли вони хочуть створити динамічний вміст.

## Як це зробити:

Різні шелы використовують різні синтаксиси для інтерполяції рядка, але у Fish Shell ви робите так:

```fish
set name "Василий"
echo "Привет, $name"
```

Цей код виведе "Привет, Василий"

## Пірнання глибше

Раніше, перед появою інтерполяції рядків, програмісти повинні були використати конкатенацію рядків для створення динамічних рядків. Але з появою інтерполяції рядків процес став більш прямолінійним і зрозумілим.

Ви також можете використовувати метод `printf` для інтерполяції рядків у Fish Shell, ось приклад:

```fish
set age 20
printf "Мені %s років\n" $age
```

Цей код виведе "Мені 20 років"

Зауважте, що використання виразу `$name` в середині рядка це спосіб, як Fish Shell обробляє інтерполяцію. Це відрізняється від інших оболонок, таких як BASH, де використовуються подвійні кавички, наприклад, `"Hello, ${name}"`.

## Дивіться також

- Офіційна документація Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Стаття про інтерполяцію рядків у різних мовах програмування: [https://en.wikipedia.org/wiki/String_interpolation](https://en.wikipedia.org/wiki/String_interpolation)