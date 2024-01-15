---
title:                "Знаходження довжини рядка"
html_title:           "Javascript: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Хочете дізнатися, скільки символів у певному рядку в Javascript? Знаходження довжини рядка може бути корисним, якщо ви працюєте з текстовими даними, такими як назви файлів або повідомлення.

## Як

```Javascript
const myString = "Привіт, світ!";
console.log(myString.length);

// Виведе: 12
```

Використовуючи вбудований метод `length`, ми можемо отримати кількість символів у рядку. Метод `length` повертає числове значення, що відображає кількість символів у рядку, включаючи пробіли та пунктуацію.

```Javascript
const myString = "Це багато символів!";
console.log(myString.length);

// Виведе: 21
```

Якщо ви працюєте з багато рядків одночасно, ви можете використовувати цей метод з циклом `for`, щоб отримати довжину кожного рядка.

```Javascript
const myStrings = ["Рядок 1", "Рядок 2", "Рядок 3"];

for (let i = 0; i < myStrings.length; i++) {
    console.log(myStrings[i].length);
}

// Виведе:
// 7
// 7
// 7
```

## Deep Dive

Метод `length` утворюється з властивості `length` рядка, яку можна отримати за допомогою точкової нотації, наприклад `myString.length`. Це властивість, а не метод, тому не потрібно викликати її з дужками, як при виклику функції.

У Javascript, `length` повертає 32-бітне ціле число, що представляє максимальну довжину рядка, яку можна зберегти у пам'яті. Зазвичай це 2^53 - 1, або приблизно 9 квінтильйонів символів. Це достатньо для більшості випадків використання рядків, але може бути корисно знати про це обмеження при роботі з особливо великими рядками.

## Дивіться також

- [Метод `length` у документації Mozilla Developer Network](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Основи роботи з рядками в Javascript](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-javascript-uk)