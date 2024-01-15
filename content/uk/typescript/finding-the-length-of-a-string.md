---
title:                "Знаходження довжини рядка"
html_title:           "TypeScript: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 
Чому: Одна з найпоширеніших задач у програмуванні полягає в тому, щоб знайти довжину рядка. Це важливо, тому що в багатьох випадках потрібно знати, скільки символів містить певний рядок, наприклад, при валідації користувачевого вводу або при обробці текстових даних.

## 
Як: Для початку, необхідно створити змінну, у яку буде зберігатися рядок, чий довжина нас цікавить. Наприклад, це може бути так:
```TypeScript
let myString = "Привіт, світ!";
```

### 
Потім збільшимо довжину рядка (у даному випадку - на одиницю), використовуючи властивість "length" таким чином:
```TypeScript
let myString = "Привіт, світ!";
console.log(myString.length); // Результат - 12
```

### 
Також можна використати цикл for для підрахунку кількості символів у рядку:
```TypeScript
let myString = "Привіт, світ!";
let count = 0; // початкове значення лічильника
for (let i = 0; i < myString.length; i++) {
  count++;
}
console.log(count); // Результат - 12
```

### 
Використання методу "split()" також може допомогти при знаходженні довжини рядка. Цей метод розбиває рядок на масив підстрок та повертає його довжину:
```TypeScript
let myString = "Привіт, світ!";
let myArray = myString.split(""); // розбиваємо рядок на масив підстрок
console.log(myArray.length); // Результат - 12
```

## 
Глибоке дослідження: Як вже згадувалося раніше, в деяких мовах програмування рядки є незмінними, тобто не можуть бути змінені. Однак, в TypeScript рядки можуть бути змінені за допомогою методу "replace()". Приклад використання:
```TypeScript
let myString = "Привіт, світ!";
myString = myString.replace("Привіт", "Вітаю"); // Змінюємо підстроку "Привіт" на "Вітаю"
console.log(myString); // Вітаю, світ!
// Результат - 12
```

### 
Додатково, TypeScript пропонує багато вбудованих методів для роботи з рядками, таких як "toUpperCase()"/"toLowerCase()", "trim()", "slice()" та ін. Детальніше про них можна дізнатися в офіційній документації.

## 
Див. також: 
- [Офіційна документація TypeScript](https://www.typescriptlang.org/docs/)
- [Стаття про роботу з рядками у TypeScript](https://www.tutorialsteacher.com/typescript/string-in-typescript)
- [Додаткові методи роботи з рядками у TypeScript](https://www.javatpoint.com/types