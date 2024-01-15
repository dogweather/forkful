---
title:                "Аналізування html"
html_title:           "TypeScript: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Why
Багато програмістів користуються парсингом HTML для отримання даних з веб-сторінок, таких як текст, зображення або посилання. Це може бути дуже корисним для отримання інформації зі сторонніх джерел або для створення власних інструментів для обробки великого обсягу даних.

## How To
Існує багато способів парсити HTML з допомогою TypeScript, але основна ідея - це використання бібліотеки для роботи з DOM (Document Object Model). Наприклад, ми можемо використовувати бібліотеку cheerio для отримання вмісту з HTML сторінки:

```TypeScript
// Встановлюємо cheerio
npm install cheerio

// Імпортуємо cheerio
import * as cheerio from 'cheerio';

// Використовуємо axios для отримання HTML сторінки
axios.get('https://www.example.com')
  .then((response) => {
    // Зберігаємо вміст сторінки у змінну
    const html = response.data;

    // Використовуємо cheerio для створення об'єкта DOM
    const $ = cheerio.load(html);

    // Отримуємо текст усіх заголовків h1 на сторінці
    const headings = $('h1').text();

    // Виводимо результат у консоль
    console.log(headings);
  })
  .catch((error) => {
    console.log(error);
  });
```

В результаті ми отримаємо текст усіх заголовків h1 на сторінці: "Hello World".

## Deep Dive
У коді вище ми використали бібліотеку cheerio для створення об'єкта DOM початкової HTML сторінки. Цей об'єкт має багато корисних методів, які дозволяють знаходити і отримувати потрібні елементи. Наприклад, використовуючи метод ```text()```, ми можемо отримати текст елементу.

Також існують інші бібліотеки, такі як jsdom, які дозволяють виконувати парсинг HTML без запуску браузера. Це дуже зручно, оскільки дозволяє уникнути додаткових витрат на час та ресурси комп'ютера.

## See Also
- Відеоурок про парсинг HTML з допомогою cheerio: https://www.youtube.com/watch?v=zp10wQker-M
- Документація з використання cheerio: https://cheerio.js.org/
- Ресурси для вивчення TypeScript: https://www.typescriptlang.org/docs/home.html