---
title:                "Javascript: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

Регулярні вирази (іноді називаються регулярними виразами або просто регулярками) - це потужний інструмент у світі програмування. Вони дають можливість знаходити і змінювати певні шаблони тексту за допомогою простого синтаксису. Використання регулярних виразів допомагає ефективно обробляти інформацію та знижує кількість потрібного коду.

## Як

Щоб почати використовувати регулярні вирази, необхідно створити об'єкт RegExp у Javascript. Цей об'єкт приймає два параметри: шаблон і флаги. Шаблон - це вираз, який шукається в тексті, а флаги визначають, як саме шаблон буде шукатися. Наприклад:

```Javascript
// Шаблон - слово "hello"
// Флаги - "g" (глобальний пошук) та "i" (регістронезалежний)
let regex = new RegExp("hello", "gi");
```

Після того як ми створили регулярний вираз, ми можемо використовувати його для пошуку та зміни в текстовій інформації. Наприклад:

```Javascript
let str = "Hello world! I am learning JavaScript.";
// Використання методу test() для перевірки наявності шаблону у тексті
console.log(regex.test(str)); // виведе true

// Використання методу replace() для заміни шаблону на інший текст
let newStr = str.replace(regex, "Hi");
console.log(newStr); // виведе "Hi world! I am learning Hi."
```

## Поглиблене вивчення

Хоча використання регулярних виразів може здатися складною, вони дозволяють робити багато речей. Наприклад, з їх допомогою можна перевіряти наявність певних символів у паролі, валідувати email-адреси, знаходити та заміняти певні слова у тексті і багато іншого. Щоб дізнатися більше про синтаксис і можливості регулярних виразів, рекомендую прочитати документацію на [MDN](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions) або [w3schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp).

## Дивіться також

- [Повний посібник по використанню регулярних виразів в Javascript](https://www.sitepoint.com/regexp-javascript/)
- [Короткий огляд регулярних виразів в Javascript](https://www.taniarascia.com/regular-expression-javascript/)
- [Основи регулярних виразів в Javascript](https://medium.com/factory-mind/regex-might-be-beautiful-but-it-is-not-easy-875f5c2feac2