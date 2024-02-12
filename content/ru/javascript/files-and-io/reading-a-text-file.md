---
title:                "Чтение текстового файла"
aliases:
- /ru/javascript/reading-a-text-file/
date:                  2024-01-29T00:01:04.212895-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Чтение текстового файла означает извлечение информации из документа .txt в вашу программу. Программисты делают это для доступа и манипулирования данными: настройки конфигурации, логи, экспорт и так далее. Просто и ясно.

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
