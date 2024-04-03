---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:51.046932-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 TypeScript \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\
  \u0432\u043A\u0438 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u043E\u0431\
  \u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ Fetch API. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u043D\u0438\u0435\u043C `async/await` \u0434\u043B\u044F \u0443\
  \u043F\u0440\u043E\u0449\u0435\u043D\u0438\u044F."
lastmod: '2024-03-13T22:44:44.583405-06:00'
model: gpt-4-0125-preview
summary: "\u0412 TypeScript \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\
  \u043A\u0438 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u043E\u0431\u044B\
  \u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ Fetch API."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
В TypeScript для отправки HTTP-запросов обычно используют Fetch API. Вот простой пример с использованием `async/await` для упрощения:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Ошибка HTTP! Статус: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Ошибка fetch:', error);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

Пример вывода для успешного запроса:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Подробнее
HTTP-запросы имеют первостепенное значение с зарождения веба; они способ общения браузеров и серверов. До появления `fetch` использовался XMLHttpRequest (XHR), который выполнял свои функции, но ощущался как бюрократия. `fetch` - это современная альтернатива, основанная на промисах, более чистая и являющаяся частью объекта window в большинстве современных браузеров.

Альтернативы `fetch` в TypeScript включают в себя библиотеки вроде Axios, которые предоставляют больше возможностей и иногда удобнее в обращении. Axios автоматически трансформирует данные JSON, обрабатывает отмену запроса и предлагает лучшее управление ошибками.

За кулисами TypeScript компилируется в JavaScript. Когда вы отправляете HTTP-запрос с использованием `fetch`, вы по сути используете родной Fetch API браузера. Проверка типов в TypeScript повышает стабильность вашего кода, вылавливая ошибки типов на этапе компиляции.

## Смотрите также
- MDN Web Docs по Fetch: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Репозиторий Axios на GitHub: https://github.com/axios/axios
- Сравнение библиотек для HTTP-запросов: https://www.npmtrends.com/axios-vs-fetch
