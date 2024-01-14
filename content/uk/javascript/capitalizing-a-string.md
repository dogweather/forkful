---
title:                "Javascript: Перетворення рядка на великі літери."
simple_title:         "Перетворення рядка на великі літери."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Чому

Не існує однозначної відповіді на запитання "навіщо зробити стрічку з великої літери" в джерелах, але це може бути корисним при написанні програми, яка перевіряє написання імен або визначає важливі слова в тексті.

##Як

```Javascript
const text = "це прикладтексту"; 
const capitalizedText = text.capitalize();
console.log(capitalizedText); // Вивід: Це прикладтексту
```

Також можна використовувати цей метод для першої літери в кожному слові в стрічці:

```Javascript
const text = "це прикладтексту"; 
const capitalizedText = text.split(" ").map(word => word.capitalize()).join(" ");
console.log(capitalizedText); // Вивід: Це Прикладтексту
```

##Глибинний розгляд

У JavaScript немає вбудованої функції для перетворення стрічки на великі літери. Тому для цього потрібно використовувати методи, які вже є в об'єкті String, такі як `split()`, `map()` і `join()`, або можна створити власну функцію capitalize. Це може бути корисно при роботі зі стрічками в програмі.

##Дивіться також

- [MDN по методу великого літеру в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Сайт з вчителем JavaScript на додатковому рівні](https://javascript.info/)