---
title:                "Видобування підрядків."
html_title:           "Gleam: Видобування підрядків."
simple_title:         "Видобування підрядків."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що & Чому?
Вирізання зазначень - це процес вибору певної частини рядка або рядка із великої текстової змінної. Програмісти часто зазначають підрядки для подальшої обробки або використання у своїх програмах.

## Як це зробити:
```Gleam
// Оскільки Gleam побудований на основі Erlang, ми можемо використовувати функції бібліотеки String для виділення зазначень.
let main = fn() {
  let original_string = "Привіт світ, це Gleam";
  let extracted_string = String.slice(6, 11, original_string);
  // Витягнута строка буде "світ"
  // Також, ми можемо використовувати RegExp для знаходження підрядків.
  let extracted_regex = RegExp.match(~r/world/, original_string);
  // Витягнута строка буде "світ"
  play extracted_string;
  // Результат виведений на екран буде "світ"
}
```

## Глибоке погруження:
Вирізання зазначень можна вважати одним з частин синтаксису рядків. Для цього завдання також можна використовувати інші мови, такі як Java чи Python. Однак, на відміну від цих мов, Gleam має простий та зрозумілий синтаксис для вирізання зазначень. Також, у Gleam є вбудована підтримка регулярних виразів, щоб знаходити підрядки за певним шаблоном.

## Дивись також:
- [Документація Gleam](https://gleam.run/)
- [Стаття про регулярні вирази в Gleam](https://gleam.run/book/tutorials_and_extras/regex.html)
- [Огляд Gleam на ресурсі Medium](https://medium.com/@hayo_rijnders/gleam-the-most-exciting-new-programming-language-in-the-erlang-ecosystem-10f353711ad8)