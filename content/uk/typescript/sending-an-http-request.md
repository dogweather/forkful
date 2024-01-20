---
title:                "Відправлення HTTP-запиту"
html_title:           "Bash: Відправлення HTTP-запиту"
simple_title:         "Відправлення HTTP-запиту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?

HTTP-запит - це процес, який веб-клієнт (наприклад, браузер) використовує, щоб отримати дані з сервера. Ми відправляємо HTTP-запити для збору даних з API, щоб надати користувачам необхідний вміст або функціональність.

## Як це зробити:

Використовуючи TypeScript, ви можете використовувати бібліотеку `axios` для відправки HTTP-запитів. Ось основний приклад:

```TypeScript
import axios from 'axios';

async function getData(){
    try {
        const response = await axios.get('https://api.example.com/data');
        console.log(response.data);
    } catch (error) {
        console.error(error);
    }
}

getData();
```

В цьому коді `axios.get()` відправляє HTTP GET-запит на URL, а потім ми обробляємо відповідь та виводимо дані.

## Поглиблений розділ 

HTTP-запити є основою веб-спілкування. Вони були винайдені у 1990-х роках як частина протоколу HTTP. Існує багато методів HTTP-запитів, включаючи GET, POST, DELETE, UPDATE та інші.

Альтернативою `axios` є вбудований в Node.js модуль `http`, хоча його використання може бути складнішим. Також доступні інші бібліотеки, наприклад `fetch` та `request`.

Щодо деталей реалізації, `axios` використовує проміси, що спрощує асинхронне програмування. Также працює в браузерах та Node.js, що робить його універсальним вибором.

## Дивись також:

1. [Робота з HTTP-запитами з допомогою Axios в JavaScript](https://www.digitalocean.com/community/tutorials/working-with-axios-as-a-http-client-in-javascript)
2. [Node.js HTTP-документація](https://nodejs.org/api/http.html)
3. [Вивчення асинхронного програмування](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)