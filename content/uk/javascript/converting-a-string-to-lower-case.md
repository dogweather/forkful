---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:38:42.077426-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?

Перетворення рядка в нижній регістр означає заміну всіх великих літер на маленькі. Програмісти це роблять для уніфікації даних, спрощення порівняння рядків і забезпечення консистентності вводу/виводу.

## Як це зробити:

```javascript
let phrase = "Привіт, Світе!";
let lowerCasePhrase = phrase.toLowerCase();
console.log(lowerCasePhrase); // Output: "привіт, світе!"
```

```javascript
let greeting = "ЗдОрОвЕнЬкІ БулИ!";
console.log(greeting.toLowerCase()); // Output: "здоровенькі були!"
```

## Підводне каміння:

Історично, метод `toLowerCase` був частиною Javascript з його перших версій. Він відноситься до прототипу String і тому доступний для всіх рядків. Є альтернативи, наприклад `toLocaleLowerCase()`, яка враховує локалізацію, тобто певні мовні особливості при переведенні у нижній регістр.

Реалізація `toLowerCase` у браузерах та серверних середовищах, як Node.js, гарантує, що вона працюватиме однаково універсально. Однак, слід звернути увагу на специфічні символи та літери, які можуть мати особливі правила перетворення в окремих мовах. Для більшості випадків `toLowerCase` повинен працювати без зайвих проблем.

## Дивіться також:

- Документація MDN за методом `toLowerCase()`: [Mozilla MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Порівняння `toLowerCase()` та `toLocaleLowerCase()`: [toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- Рекомендації по стилі коду з Airbnb, де згадується використання методів рядків: [Airbnb JavaScript Style Guide](https://github.com/airbnb/javascript)