---
title:                "Отправка HTTP-запроса"
aliases: - /ru/typescript/sending-an-http-request.md
date:                  2024-01-29T00:02:51.046932-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Отправка HTTP-запроса — это способ, которым ваша программа запрашивает данные с сервера или отправляет данные на сервер. Программисты делают это, потому что это основа взаимодействия с веб-сервисами, API и удаленными ресурсами.

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
