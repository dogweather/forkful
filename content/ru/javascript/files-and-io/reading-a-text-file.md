---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:04.212895-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u043C\u043E\u0436\u043D\u043E\
  \ \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0442\u044C \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u044B\u0439 \u0444\u0430\u0439\u043B \u0432 \u0441\u043E\u0432\
  \u0440\u0435\u043C\u0435\u043D\u043D\u043E\u043C JavaScript: **\u0418\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435 Node.js \u0441 \u041F\
  \u0440\u043E\u043C\u0438\u0441\u0430\u043C\u0438 (Async/Await)**."
lastmod: '2024-03-13T22:44:45.791771-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u043C\u043E\u0436\u043D\u043E \u043F\
  \u0440\u043E\u0447\u0438\u0442\u0430\u0442\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u044B\u0439 \u0444\u0430\u0439\u043B \u0432 \u0441\u043E\u0432\u0440\u0435\
  \u043C\u0435\u043D\u043D\u043E\u043C JavaScript."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

## Как это сделать:
Вот как можно прочитать текстовый файл в современном JavaScript:

**Использование Node.js с Промисами (Async/Await)**:

```javascript
const fs = require('fs').promises;

async function readFile(filePath) {
  try {
    const data = await fs.readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error('Произошла ошибка при попытке чтения файла:', error);
  }
}

readFile('example.txt');
```

Пример вывода (содержимое `example.txt`):

```
Привет, это текстовый файл!
```

**Использование API fetch в браузере**:

```javascript
async function fetchTextFile(fileUrl) {
  try {
    const response = await fetch(fileUrl);
    const text = await response.text();
    console.log(text);
  } catch (error) {
    console.error('Упс, что-то пошло не так при попытке получить файл:', error);
  }
}

fetchTextFile('example.txt');
```

## Глубокое погружение
Изначально, чтение файлов в JavaScript было в основном делом серверной стороны, с которым справлялся Node.js. По мере того как JS вошел в браузеры вместе с HTML5, появились такие API, как `FileReader` и `fetch`, сделавшие чтение файлов на стороне клиента возможным без особых усилий.

Альтернативы? О, их несколько. Потоки (Streams) могут обрабатывать большие файлы без захвата всей памяти. Воркеры предотвращают зависание пользовательского интерфейса. Библиотеки упрощают выполнение сложных задач. У каждого из них есть свое место.

Внутри, чтение файла может включать в себя управление буфером, кодировку символов (UTF-8 и т.д.) и обработку ошибок. Будьте внимательны к безопасности, также; браузеры ограничивают доступ к файлам по веским причинам.

## Смотрите также
Расширьте свои знания с помощью этих ресурсов:

- Документация по API FileReader от MDN: [MDN FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Документация по файловой системе Node.js: [Node.js fs](https://nodejs.org/api/fs.html)
- Stream API для больших файлов: [Node.js stream](https://nodejs.org/api/stream.html)
- Понимание API fetch: [MDN fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
