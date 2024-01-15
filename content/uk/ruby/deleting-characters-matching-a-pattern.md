---
title:                "Видалення символів, що відповідають паттерну"
html_title:           "Ruby: Видалення символів, що відповідають паттерну"
simple_title:         "Видалення символів, що відповідають паттерну"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Існує багато причин, чому люди можуть бажати видаляти символи, які відповідають певному шаблону. Наприклад, можна застосовувати цей метод для очищення введення користувачів або для видалення ненужних елементів зі сторінок веб-сайту.

## Як

```Ruby
# Створюємо метод для видалення символів за певним шаблоном
def delete_matching_characters(input_string, pattern)
    # Використовуємо метод gsub для заміни символів
    input_string.gsub(pattern, "")
end

# Приклад використання методу
string = "Привіт! Я дуже щасливий!"
puts delete_matching_characters(string, /[а-яіїє]+/) # Видалить всі українські літери
# Виведе: "! ! "

puts delete_matching_characters(string, /[А-ЯІЇЄ]+/) # Видалить всі великі букви
# Виведе: "ривіт! я дужн щасливй!"

puts delete_matching_characters(string, /[.,!]/) # Видалить всі пунктуаційні знаки
# Виведе: "Привіт Я дуже щасливий"

```

## Глибоке занурення

За допомогою методу `gsub` можна видалити не тільки окремі символи, але й цілі рядки, що відповідають шаблону. Також можна поєднувати різні шаблони, застосовуючи спеціальні символи, які вказують на вказані позиції у рядку.

## Дивись також

- [Документація Ruby для методу `gsub`](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [Відеоурок для початківців про використання методу `gsub`](https://www.youtube.com/watch?v=AZEVfWWU5ZU)
- [Теорія про регулярні вирази](https://www.regextester.com/learn-regex) (українською)