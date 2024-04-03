---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:22.477685-07:00
description: "\u041A\u0430\u043A: \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\
  \u044B\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0441\u043A\u0430\u0447\u0430\
  \u0442\u044C \u0441\u0442\u0440\u0430\u043D\u0438\u0446\u0443 \u0441 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C Node.js\
  \ \u0438 `node-fetch`."
lastmod: '2024-03-13T22:44:45.758177-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u0441\u043A\u0430\u0447\u0430\u0442\u044C \u0441\u0442\
  \u0440\u0430\u043D\u0438\u0446\u0443 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C Node.js \u0438 `node-fetch`."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как:
Вот быстрый способ скачать страницу с использованием Node.js и `node-fetch`:

```Javascript
const fetch = require('node-fetch'); // сначала вам может потребоваться установить этот пакет!

async function downloadPage(url) {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // Выводит исходный HTML код страницы
    } catch (error) {
        console.error(error);
    }
}

downloadPage('https://example.com');
```

Пример вывода:

```
<!doctype html>
<html>
<head>
    <title>Пример домена</title>
...
</html>
```

## Подробнее
Ранее скачивание веб-страницы осуществлялось с использованием XMLHTTPRequest в браузере или модуля `http` в Node.js. Однако после введения ES6, `fetch` API стал современным стандартом благодаря более простому синтаксису и основанной на промисах природе.

К альтернативам относится `axios`, популярный npm пакет, который обрабатывает запросы с немного большей функциональностью, чем родной fetch. Для сложных случаев использования может потребоваться `puppeteer` для рендеринга страницы в бесголовом браузере, что полезно для работы с содержимым, отрисованным с помощью JavaScript.

При реализации загрузки страниц обращайте внимание на аспекты, такие как уважение к `robots.txt`, управление `User-Agent` для избежания блокировки, и тщательное управление асинхронностью, чтобы избежать потенциальных проблем с перегрузкой сервера или условиями гонки.

## Смотрите также
- MDN Web Docs по API `fetch`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Страница GitHub `axios`: https://github.com/axios/axios
- Страница GitHub `puppeteer`: https://github.com/puppeteer/puppeteer
- Статья о лучших практиках веб-скрейпинга: https://www.scrapingbee.com/blog/web-scraping-best-practices/
