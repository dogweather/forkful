---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:06.219733-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0437\u0430\u0433\
  \u0440\u0443\u0437\u0438\u0442\u044C \u0432\u0435\u0431-\u0441\u0442\u0440\u0430\
  \u043D\u0438\u0446\u0443 \u043D\u0430 TypeScript, \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u044F Node.js \u0438 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0443 `node-fetch`. \u0412\u043E\u0442 \u043A\u0430\u043A."
lastmod: '2024-03-13T22:44:44.586822-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0437\u0430\u0433\u0440\
  \u0443\u0437\u0438\u0442\u044C \u0432\u0435\u0431-\u0441\u0442\u0440\u0430\u043D\
  \u0438\u0446\u0443 \u043D\u0430 TypeScript, \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u044F Node.js \u0438 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0443 `node-fetch`."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Вы можете загрузить веб-страницу на TypeScript, используя Node.js и библиотеку `node-fetch`. Вот как:

```TypeScript
import fetch from 'node-fetch';

async function downloadWebPage(url: string): Promise<void> {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // Выводит HTML содержимое в консоль
    } catch (error) {
        console.error('Не удалось загрузить:', error);
    }
}

// Использование функции
downloadWebPage('https://example.com');
```

Пример вывода (обрезанный):
```
<!doctype html>
<html>
<head>
    <title>Пример домена</title>
...
</html>
```

## Глубокое погружение
Исторически веб-контент загружался с использованием таких инструментов, как `wget` или `curl` в командной строке. Однако, в современном программировании у нас есть библиотеки такие как `node-fetch`, `axios` или `request` (устаревшая, но все еще используемая), которые предоставляют больше функциональности и легче интегрируются в наши JavaScript/TypeScript приложения.

При загрузке веб-страницы есть не только HTML. CSS, JavaScript, изображения и другие ресурсы также входят в комплект. Обычно сначала получают только HTML, а затем любая дополнительная обработка или загрузка определяется тем, что вам нужно со страницы.

С точки зрения реализации, `node-fetch` по сути представляет собой API window.fetch для Node.js. Он возвращает промис, который разрешается в ответ на запрос, позволяя вам получить поток текста (.text()), объект JSON (.json()) или даже буфер (.buffer()) для двоичных данных.

Имейте в виду, что права на веб-скрапинг определяются файлом `robots.txt` веб-сайта и условиями обслуживания. Всегда проверяйте, разрешено ли вам скрапинг сайта, и соблюдайте лимиты скорости, чтобы избежать юридических проблем или блокировки вашего IP.

## Смотрите также
- [Документация `node-fetch`](https://github.com/node-fetch/node-fetch)
- [MDN Веб-документация по Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Библиотека `axios`](https://github.com/axios/axios)
- [HTTP статус коды (для обработки ответов)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [Законность веб-скрапинга](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right/)
