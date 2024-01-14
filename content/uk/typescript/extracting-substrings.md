---
title:    "TypeScript: Видобування підрядків"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Зачем

Існує багато ситуацій, коли потрібно виділити частину рядка, щоб зробити певні обчислення або перетворення. Наприклад, ви можете потребувати перевірити, чи містить рядок певний формат, чи вилучити певну інформацію для подальшого використання. Для цих завдань нам потрібно вміти дістати певну підстроку зі строки, і саме про це ми поговоримо у цій статті.

## Як

Існує кілька способів отримати підстроку зі строки в TypeScript, і ми розглянемо декілька з них.

### Метод `substring`

Метод `substring` використовується для отримання підстроки з вказаного індексу по закінчення заданої довжини.

```TypeScript
const str = "Hello World";

// Отримання підстроки "World"
const substr1 = str.substring(6, 11);

// Отримання підстроки "Hello"
const substr2 = str.substring(0, 5);
```

### Метод `substr`

Метод `substr` отримує підстроку по заданому початковому індексу та довжині.

```TypeScript
const str = "Hello World";

// Отримання підстроки "World"
const substr1 = str.substr(6, 5);

// Отримання підстроки "Hello"
const substr2 = str.substr(0, 5);
```

### Регулярні вирази

Регулярні вирази дозволяють задати шаблон, за яким потрібно шукати певну підстроку у рядку. Один із варіантів використання регулярних виразів - це вилучення інформації зі строки за допомогою методу `match`.

```TypeScript
// Отримання номеру телефону з формату +380(XX)XXXXXXX
const str = "My number is +380(63)1234567";

const phoneRegex = /\+380\((\d{2})\)(\d{7})/;
const phoneMatch = str.match(phoneRegex);

// phoneMatch = ["+380(63)1234567", "63", "1234567"]
const phone = phoneMatch[1] + phoneMatch[2];
```

## ",

У цій статті ми детально розглянули різні способи отримання підстроки зі строки в TypeScript. Залежно від завдання, ви можете використовувати один з цих методів або комбінувати їх для досягнення потрібного результату.

## Ресурси

- [Офіційна документація з методами роботи зі строками в TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Підстроки в TypeScript (Medium, англійською)](https://medium.com/@neculaesei/understanding-substring-in-javascript-5904b0746506)
- [Регулярні вирази в TypeScript (SitePoint, ангійською)](https://www.sitepoint.com/ultimate-guide-to-regular-expressions-in