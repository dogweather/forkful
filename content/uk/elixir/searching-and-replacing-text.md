---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Пошук та заміна тексту - це процес виявлення певного шаблону або фрази у тексті та його заміна. Програмісти роблять це для автоматизації та оптимізації процесів редагування коду.

## Як це зробити:

Ось простий приклад, як це можна реалізувати в Elixir за допомогою функції `String.replace/3`.

```elixir
original_string = "Hello, World"
new_string = String.replace(original_string, "World", "Elixir")
IO.puts new_string
```

В результаті ви отримаєте:

```
"Hello, Elixir"
```

## Поглиблений огляд:

1. Історичний контекст: Пошук та заміна тексту є основним функционалом, що присутній у більшості мов програмування з самого початку.

2. Альтернативи: Elixir також представляє функцію `String.split/1`, яка розділяє рядок за певним шаблоном, а потім об’єднує його назад з використанням `Enum.join/1` і новим значенням.

3. Роз'яснення деталей реалізації: Функція `String.replace/3` у Elixir працює шляхом перетворення рядка на список символів, пошуку шаблону, його заміни та об'єднання списоку назад у рядок.

## Дивіться також:

- Офіційна документація Elixir: https://hexdocs.pm/elixir/String.html
- Практичний підхід до рядків у Elixir: https://joaquimadraz.com/elixir/command-lines-string-manipulation/
- Руководство по регулярних виразах в Elixir: https://elixir-lang.org/getting-started/regexes-and-pattern-matching.html