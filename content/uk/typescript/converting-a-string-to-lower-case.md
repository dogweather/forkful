---
title:                "TypeScript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Why: Чому

Уявіть, що у вас є програма, яка отримує введений користувачем текст і повинна обробити його у певному форматі. Однак, якщо користувач введе текст у верхньому регістрі, це може спричинити проблеми при подальшій обробці. Тому, перетворення рядка до нижнього регістру може бути корисним кроком для забезпечення правильної роботи вашої програми.

How To: Конвертування рядка до нижнього регістру досить просте в TypeScript. Просто використайте метод `toLowerCase ()` вбудованого об'єкта `String` для зміни всіх символів рядка на малі літери.

```TypeScript
const string = "Привіт, Я TypeScript Рядок";

console.log(string.toLowerCase());
// виведе "привіт, я typescript рядок"
```

Deep Dive: Починаючи з TypeScript 4.1, можна використовувати ключове слово `as const` для додаткової безпеки при конвертуванні рядків до нижнього регістру. Це забезпечує, що тип рядка не буде змінений після конвертування.

```TypeScript
const string = "Привіт, я TypeScript Рядок" as const;

console.log(string.toLowerCase());
// виведе "привіт, я typescript рядок"

console.log(string.toUpperCase());
// отримаємо помилку, оскільки тип "string" був збережений як постійний
```

See Also (Дивись також):
- [Метод toLowerCase() в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Огляд TypeScript 4.1](https://devblogs.microsoft.com/typescript/announcing-typescript-4-1/)