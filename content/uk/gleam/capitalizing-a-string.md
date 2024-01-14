---
title:    "Gleam: Капіталізація рядка"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Чому

Збільшення першої літери рядка є однією з найпоширеніших операцій при роботі з текстовими даними. Використання цієї функції може допомогти вам поліпшити вигляд тексту та зробити його більш зрозумілим.

## Як

```Gleam
let text = "це рядок для збільшення першої літери"

let capitalized = String.capitalize(text)

// Результат: "Це рядок для збільшення першої літери"
```

```Gleam
// Можна також використовувати цю функцію для збільшення першої літери окремих слів в реченні

let sentence = "це просте речення для прикладу"

let capitalized = String.capitalize_words(sentence)

// Результат: "Це Просте Речення Для Прикладу"
```

## Глибокий погляд

Функція `capitalize` використовує мовні налаштування, щоб визначити, які символи повинні бути великими, а які - малими. Таким чином, вона працює коректно незалежно від мови, в якій написаний рядок.

## Дивись також

- [Документація Gleam String модуля](https://gleam.run/play/?code=delegate%20String%20%7B%0A%20%20capitalize%20%3A%20Utf8String%20-%3E%20Utf8String%0A%20%20capitalize_words%20%3A%20Utf8String%20-%3E%20Utf8String%0A%7D%0A%0Aexternal%20String.capitalize%20%3A%20Utf8String%20-%3E%20Utf8String%20-%3E%20String.normalize%0Aexternal%20String.capitalize_words%20%3A%20Utf8String%20-%3E%20Utf8String%20-%3E%20String.normalize%0A%0Afn%20String.capitalize%20str%20%7B%0A%20%20delegate%20str%0A%7D%0A%0Afn%20String.capitalize_words%20str%20%7B%0A%20%20delegate%20str%20%7C%3E%20String.split_words%20%7C%3E%20List.map%20String.capitalize%20%7C%3E%20String.join_words%0A%7D%0A%0AString.capitalize%20%22hello%20world%22%0A%0AString.capitalize_words%20%22hello%20world%22)
- [Відеоурок з навчання основам Gleam](https://www.youtube.com/watch?v=g05QlRR9yrg)
- [Gleam проект на GitHub](https://github.com/gleam-lang/gleam)