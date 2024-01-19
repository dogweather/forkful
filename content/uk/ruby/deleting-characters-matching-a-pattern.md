---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Видалення символів, що відповідають певному шаблону в Ruby

## Що це і навіщо?

Видалення символів, що відповідають певному шаблону - це процес, коли програма видаляє певні символи з рядка на основі заданого шаблону. Програмісти роблять це для очищення даних, виправлення помилок вводу та оптимізації виконання.

## Як це зробити:

```ruby
original_string = "Hello, World!"
modified_string = original_string.delete(",!")
puts modified_string  # Виводить "Hello World"
```
Вище наведений приклад демонструє, як можна видалити коми і знаки оклику зі стрічки в Ruby, використовуючи метод `delete`.

## Поглиблений огляд:

Ruby була створена в 1995 році Юкіхіро Мацумото. Цей набір засобів для обробки рядків був однією з основних функцій мови.

Існує кілька альтернатив видаленню символів, наприклад метод `gsub`, який дозволяє видалити символи, замінюючи їх на інші.

Метод `delete` працює шляхом перебору кожного символу в рядку і видалення тих, які відповідають певному шаблону. 

## Дивіться також:

Ruby Doc - Стрічки: [https://ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)

Розуміння стрічок в Ruby: [https://www.rubyguides.com/2015/05/working-with-strings/](https://www.rubyguides.com/2015/05/working-with-strings/)