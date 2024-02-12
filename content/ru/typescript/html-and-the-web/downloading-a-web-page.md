---
title:                "Загрузка веб-страницы"
aliases:
- /ru/typescript/downloading-a-web-page/
date:                  2024-01-28T23:58:06.219733-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Загрузка веб-страницы означает получение HTML, CSS и, возможно, других ресурсов с URL, по которому вы переходите. Программисты делают это для обработки содержимого, сбора данных, проверки наличия обновлений или для кэширования веб-сайтов для офлайн использования.

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
