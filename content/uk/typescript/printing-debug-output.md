---
title:                "TypeScript: Друкування відладочного виведення"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

Ви часто стикаєтесь з недоліками в коді і бажаєте побачити, що саме відбувається на кожному етапі виконання програми? На допомогу приходить друкування відладочних повідомлень, яке дозволяє стежити за процесом виконання програми та здійснювати корекції вчасно. У цій статті ми розкажемо, як правильно друкувати відлагочні повідомлення в TypeScript.

## Як

Для виводу дебриг-повідомлень у TypeScript використовуйте команду ```console.log()```. Нижче наведено приклад коду, який друкуює повідомлення про привітання:

```TypeScript
console.log("Привіт, Світ!");
```

Ви можете друкувати будь-які дані, включаючи змінні, об'єкти та функції. Наприклад:

```TypeScript
let language = "TypeScript";
console.log("Я використовую мову " + language);
```

Цей код виведе повідомлення "Я використовую мову TypeScript".

## Вдосконалена розробка

Якщо ви хочете детальніше дослідити процес друкування відлагочних повідомлень, то в TypeScript є ще декілька корисних команд. Наприклад, ```console.error()``` дозволяє вивести повідомлення про помилку, а ```console.warn()``` - про попередження. Також ви можете додати додаткові параметри до команди, наприклад, ID або назву тега, щоб краще відслідковувати та фільтрувати виведення.

## Дивіться також

- [Документація TypeScript](https://www.typescriptlang.org/docs/home.html)
- [Стаття про відлагочне програмування в TypeScript](https://velog.io/@jakeseo_me/Debugging-Your-Typescript)
- [Підручник з TypeScript для початківців](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)