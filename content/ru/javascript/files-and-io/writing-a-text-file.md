---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:25.322473-07:00
description: "\u041A\u0430\u043A: JavaScript \u0432 \u0431\u0440\u0430\u0443\u0437\
  \u0435\u0440\u0435 \u043D\u0435 \u0438\u043C\u0435\u0435\u0442 \u043F\u0440\u044F\
  \u043C\u043E\u0433\u043E \u0434\u043E\u0441\u0442\u0443\u043F\u0430 \u043A \u0444\
  \u0430\u0439\u043B\u043E\u0432\u043E\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u0435\
  \ \u043F\u043E \u0441\u043E\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u0438\u044F\
  \u043C \u0431\u0435\u0437\u043E\u043F\u0430\u0441\u043D\u043E\u0441\u0442\u0438\
  . \u041D\u043E \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0441\u043E\u0437\
  \u0434\u0430\u0442\u044C \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0439\
  \ \u0444\u0430\u0439\u043B \u0438 \u043F\u0440\u0435\u0434\u043B\u043E\u0436\u0438\
  \u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:45.793505-06:00'
model: gpt-4-0125-preview
summary: "JavaScript \u0432 \u0431\u0440\u0430\u0443\u0437\u0435\u0440\u0435 \u043D\
  \u0435 \u0438\u043C\u0435\u0435\u0442 \u043F\u0440\u044F\u043C\u043E\u0433\u043E\
  \ \u0434\u043E\u0441\u0442\u0443\u043F\u0430 \u043A \u0444\u0430\u0439\u043B\u043E\
  \u0432\u043E\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u0435 \u043F\u043E \u0441\
  \u043E\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u0438\u044F\u043C \u0431\u0435\u0437\
  \u043E\u043F\u0430\u0441\u043D\u043E\u0441\u0442\u0438."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 24
---

## Как:
JavaScript в браузере не имеет прямого доступа к файловой системе по соображениям безопасности. Но вы можете создать текстовый файл и предложить пользователю сохранить его:

```javascript
function downloadTextFile(text, filename) {
  const blob = new Blob([text], { type: 'text/plain' });
  const a = document.createElement('a');
  a.download = filename;
  a.href = window.URL.createObjectURL(blob);
  a.dataset.downloadurl = ['text/plain', a.download, a.href].join(':');
  a.style.display = "none";
  document.body.appendChild(a);  // Добавляем якорь к телу документа.
  a.click();
  
  document.body.removeChild(a);  // Уборка якоря после использования.
  window.URL.revokeObjectURL(a.href);  // Освобождаем URL blob.
}

// Пример использования:
downloadTextFile('Привет, мир!', 'example.txt');
```

Node.js предоставляет более простой способ записи файлов через модуль `fs`:

```javascript
const fs = require('fs');

fs.writeFile('example.txt', 'Привет, мир!', (err) => {
  if (err) throw err;
  console.log('Файл был сохранен!');
});
```

## Погружение
Исторически JavaScript был ограничен браузером без доступа к файловой системе. Node.js изменил это, предоставив возможности на стороне сервера.

Альтернативы `fs.writeFile` включают `fs.writeFileSync` для синхронных операций и `fs.promises.writeFile` для асинхронного контроля на основе промисов.

Методы `fs` в Node обрабатывают буферы и потоки — инструменты для работы с большими файлами и сетевым общением.

## См. также
- Документация по файловой системе Node.js: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN - Blob: [https://developer.mozilla.org/en-US/docs/Web/API/Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
- Руководство по JavaScript на MDN: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
