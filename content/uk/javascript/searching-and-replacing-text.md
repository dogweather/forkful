---
title:                "Javascript: Пошук і заміна тексту"
simple_title:         "Пошук і заміна тексту"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому?

Програмування - складний процес іноді вимагає зайняти велику кількість часу. Однак іноді навіть проста задача, така як заміна тексту, може бути викликом. Тому, знання як шукати та замінювати текст у своєму коді може суттєво полегшити вашу роботу та зекономити багато часу.

## Як це зробити?

Існує багато різних способів шукати та замінювати текст у Javascript. Один з найпростіших способів - використовувати метод `.replace()`.

```Javascript
let str = "Це простий рядок тексту.";
let newStr = str.replace("простий", "складний");
console.log(newStr);
```

Результатом буде `"Це складний рядок тексту."`, оскільки метод `.replace()` змінює перше входження шуканого тексту на запропонований.

Ви також можете використовувати регулярні вирази для пошуку та заміни більш складних шаблонів тексту.

```Javascript
let str = "Hello, world!";
let newStr = str.replace(/Hello/g, "Привіт");
console.log(newStr);
```

Результатом буде `"Привіт, world!"`, оскільки `/Hello/g` означає шукати всі входження тексту "Hello" у рядку та замінювати їх на "Привіт".

## Глибше в деталі

Шукання та заміна тексту може бути використана не тільки для простих замін, але і для валідації вводу користувача, зміни формату даних та багатьох інших сценаріїв. Крім того, в Javascript є багато інших методів для роботи з рядками, які можуть допомогти у шуканні та заміні тексту, наприклад, `.indexOf()`, `.split()`, `.match()` та інші. Шукаючи та вивчаючи ці методи, ви зможете стати більш ефективним програмістом.

## Дивіться також

- [MDN Документація про метод .replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Регулярні вирази в Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools Рядки у Javascript](https://www.w3schools.com/js/js_strings.asp)