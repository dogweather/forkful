---
title:                "Пошук та заміна тексту"
html_title:           "Gleam: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому? 

Пошук та заміна тексту - це стандартна практика серед програмістів, яка допомагає знаходити та замінювати шматки коду або тексту у всьому проекті. Це зберігає час та зусилля, особливо коли потрібно змінити однакову частину коду багато разів.

## Як це зробити:

```
Gleam.string.replace(old_string, new_string, text)
```

Цей код замінює всі входження `old_string` на `new_string` у `text` і повертає оновлений текст. Наприклад:

```
Gleam.string.replace("Hello", "Hi", "Hello world!") // Output: "Hi world!"
```

Якщо потрібно зігнорувати регістр, можна використовувати `replace_case_insensitive` замість `replace`. 

## Глибока пірсвітвалість:

Пошук та заміна тексту стала популярною технікою завдяки своїй ефективності в роботі з багатими обсягами коду. Існують інші підходи, які також можуть бути використані, наприклад, використання регулярних виразів або вбудованих функцій для пошуку та заміни тексту. 

Якщо ви бажаєте отримати додаткову інформацію про заміну тексту в Gleam, можна переглянути офіційну документацію: https://gleam.run/standard-library/#string 

## Дивись також:

https://gleam.run/ - Офіційний сайт Gleam.

https://gleam.run/documentation/ - Офіційна документація Gleam.

https://www.regular-expressions.info/tutorial.html - Навчальний матеріал з регулярних виразів.