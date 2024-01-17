---
title:                "Запис у верхньому регістрі рядка"
html_title:           "Gleam: Запис у верхньому регістрі рядка"
simple_title:         "Запис у верхньому регістрі рядка"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?
Капіталізація рядка означає перетворення першої літери кожного слова в рядку на велику. Це зроблено для полегшення читання або використання рядка в якості заголовка. Програмісти часто капіталізують рядки, щоб покращити читабельність і зручність роботи з кодом.

Як це зробити:
```Gleam
string = "привіт світ!"
capitalized_string = String.capitalize(string)
```
Вивід: "Привіт Світ!"

Глибоке занурення:
Капіталізацію рядків можна виконати за допомогою різних методів, таких як використання вбудованих функцій або написання власної функції. Існує також альтернативний метод капіталізації рядків, який використовує регулярні вирази. У Gleam існує бібліотека з різними функціями капіталізації рядків, яка дозволяє вибрати найбільш зручний для вас метод.

Дивіться також:
Доповнюючі матеріали та джерела з використанням капіталізації рядків: [Gleam документація](https://gleam.run/libraries/string#String.capitalize), [stackoverflow поради](https://stackoverflow.com/questions/62388484/how-to-have-a-capitalize-function-in-gleam), [регулярні вирази в Gleam](https://dcook.org/blog/2019/09/15/gleam_collaborate_re_quit_easily/readme.html).