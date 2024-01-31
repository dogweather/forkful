---
title:                "Создание текстового файла"
date:                  2024-01-29T00:05:25.322473-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание текстового файла"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/writing-a-text-file.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Запись текстового файла на JavaScript обычно означает создание и сохранение данных в файле в формате, удобном для чтения человеком. Программисты делают это для сохранения данных, таких как настройки, логи или вывод пользователей.

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
