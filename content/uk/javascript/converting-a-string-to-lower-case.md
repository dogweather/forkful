---
title:                "Javascript: Перетворення рядка в нижній регістр"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Конвертування рядка в нижній регістр може знадобитись, якщо ви хочете змінити регістр введеного користувачем тексту, зробити його більш однорідним або зробити пошук більш гнучким.

## Як це зробити

```Javascript
let str = "Приклад Тексту";

console.log(str.toLowerCase());
```

Вивід: "приклад тексту"

Ви можете використовувати метод `toLowerCase()` для будь-якого рядка, щоб конвертувати його в нижній регістр. Цей метод повертає новий рядок зі зміненим регістром.

## Глибокий аналіз

При виклику методу `toLowerCase()`, всі букви рядка перетворюються на малі літери. Це може бути корисно при порівнянні рядків, оскільки вони будуть рівні, незалежно від поточного регістру. Також, метод `toLowerCase()` не впливає на розділові знаки та цифри, тому вони залишаються без змін.

## Дивись також

- [Стрічки в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Регулярні вирази в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions)