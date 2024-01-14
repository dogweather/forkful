---
title:    "Javascript: Удалення символів, що відповідають шаблону."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому
Деякі програмісти можуть бути зацікавлені у тому, щоб видаляти символи, які відповідають певному шаблону, у своїх програмах для поліпшення продуктивності або оптимізації коду.

## Як це зробити
Для видалення символів, які відповідають певному шаблону, нам знадобиться використовувати метод `replace ()` у поєднанні з регулярним виразом. Давайте подивимося на приклад:

```Javascript
let str = "Hello World! This is a sample string!";
let newStr = str.replace(/[A-Z]/g, ""); 
console.log(newStr); // Output: "ello orld! his is a sample tring!"
```

У цьому прикладі ми використали регулярний вираз `/[A-Z]/g` для видалення всіх заголовних літер з рядка `str` і отримали змінений рядок `newStr`.

## Глибші деталі
Видалення символів за допомогою регулярних виразів може бути потужною та ефективною технікою у програмуванні. Регулярні вирази (вирази для визначення шаблонів у тексті) дозволяють здійснювати складні пошуки та заміни з мінімальним зусиллям.

Проте, варто пам'ятати, що використання регулярних виразів може бути складним та вимагати певного досвіду. Тому перед тим, як використовувати їх для видалення символів, необхідно докладно розібратися зі структурою та синтаксисом регулярних виразів.

## Дивіться також
- [Javascript регулярні вирази: посібник для початківців](https://www.digitalocean.com/community/tutorials/javascript-regular-expressions-for-beginners)
- [Метод `replace()` у Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Регулярні вирази у Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions)