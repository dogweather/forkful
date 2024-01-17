---
title:                "Пошук та заміна тексту"
html_title:           "Javascript: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і Чому?
Заміна тексту є процесом, який дозволяє програмістам знаходити та змінювати певні фрагменти тексту в програмному коді. Часто це використовується для рефакторингу коду, виправлення помилок або оновлення вже існуючого функціоналу.

## Як це зробити:
Щоб заміняти текст у своєму Javascript коді, спочатку необхідно зрозуміти, яка саме заміна вам потрібна. Найпростіший спосіб - використовувати глобальну функцію replace(). Наприклад, уявімо, що ми маємо рядок зі словом "привіт" і хочемо замінити його на "привітання". Ми можемо використати наступний код:

```Javascript
let str = "Привіт, Як справи?";
let newStr = str.replace("привіт, "привітання");
// Результат: "Привітання, як справи?"
```

Метод replace() приймає два аргументи - перший це паттерн, який потрібно замінити, а другий - новий текст для заміни.

## Глибше копання:
Заміна тексту є важливою частиною програмування і неодмінно пов'язана з рефакторингом - процесом поліпшення і оптимізації коду. Існують інші методи заміни тексту у Javascript, такі як split() або substring(), але зазвичай вони не такі ефективні як replace(). Крім того, інші мови програмування також мають свої власні функції для заміни тексту, тож залежно від задачі можуть бути кращими варіантами.

## Дивись також:
- [MDN - replace()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3 Schools - JavaScript Replace](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Codewars - Replacing in a string](https://www.codewars.com/kata/replacing-in-a-string/javascript)