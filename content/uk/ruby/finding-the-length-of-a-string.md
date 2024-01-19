---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що та Навіщо?
Для пошуку довжини рядка використовується метод `length`, що подає кількість символів у рядку. Це корисно для перевірки введення даних, обмеження тексту та інших задач.

## Як це робити:
Маємо простий приклад коду в Ruby:
```Ruby
phrase = "Привіт, Ruby"
puts phrase.length
```
Після виконання цього коду отримаємо вихідні дані:
```
11
```
Це означає, що у рядку "Привіт, Ruby" 11 символів.

## Поглиблено:
Така можливість визначення довжини рядка є в Ruby від самого початку. Однак, важливо зазначити, що `length` підсумовує кількість символів Unicode, а не байтів. Для отримання байтів використовуйте `bytesize`.
Також в Ruby для отримання довжини рядка можна використовувати метод `size`.

Реалізація цих методів у вихідному коді розташована в `string.c`.

## Подивіться ще:
- Офіційна документація Ruby для [String#length](https://ruby-doc.org/core-2.7.1/String.html#method-i-length)
- Матеріал StackOverflow: [What is the difference between String length and size in Ruby?](https://stackoverflow.com/questions/6083219/what-is-the-difference-between-string-length-and-size-in-ruby)