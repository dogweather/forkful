---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?
Об'єднання рядків - це процес злиття двох або більше рядків у один. Програмісти роблять це для гнучкого створення складних рядків без потреби в перезаписуванні чи дублюванні коду.

## Як це робити:
Нижче наведено приклади коду на JavaScript і вихідних даних.
```Javascript
// Method 1: Using '+' operator
let string1 = "Привіт, ";
let string2 = "Світ!";
let greeting = string1 + string2;
console.log(greeting); // "Привіт, Світ!"

// Method 2: Using concat() method
let string3 = "Насолоджуйся, ";
let string4 = "програмування!";
let message2 = string3.concat(string4);
console.log(message2); // "Насолоджуйся, програмування!"
```

## Поглиблений матеріал
Метод об'єднання рядків існує в JavaScript з його задуму, оскільки він є фундаментальною функцією в більшості мов програмування. Альтернативою безпосередньому чи методу `concat()` є метод `join()`, який може бути корисним для об'єднання великої кількості рядків. Об'єднання рядків в JavaScript відбувається за допомогою бінарного `+` оператора або за допомогою методу об'єкта `String.concat()`, що бере два рядки і повертає новий, що є їхнім поєднанням.

```Javascript
// Using join() method
let stringsArray = ["Привіт, ", "Світ!", " Насолоджуйся, ", "програмування!"];
let message3 = stringsArray.join("");
console.log(message3); // "Привіт, Світ! Насолоджуйся, програмування!"
```

## Дивіться також:
1. [Mozilla Developer Network (MDN) - String.prototype.concat()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
2. [Mozilla Developer Network (MDN) - Array.prototype.join()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
3. [JavaScript Info - Basic string operations](https://javascript.info/string#concatenation-assignment-operator)