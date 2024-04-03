---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:00.633928-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Node.js, \u043E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 JavaScript \u0441\
  \u0430\u043C \u043F\u043E \u0441\u043E\u0431\u0456 \u043D\u0435 \u043C\u0430\u0454\
  \ \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u0434\u043E\u0441\u0442\u0443\u043F\
  \u0443 \u0434\u043E \u0444\u0430\u0439\u043B\u043E\u0432\u043E\u0457 \u0441\u0438\
  \u0441\u0442\u0435\u043C\u0438, \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\
  \u044C\u0441\u044F \u043C\u043E\u0434\u0443\u043B\u044C `fs` \u0434\u043B\u044F\
  \ \u0442\u0430\u043A\u0438\u0445\u2026"
lastmod: '2024-03-13T22:44:50.018938-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Node.js, \u043E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 JavaScript\
  \ \u0441\u0430\u043C \u043F\u043E \u0441\u043E\u0431\u0456 \u043D\u0435 \u043C\u0430\
  \u0454 \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u0434\u043E\u0441\u0442\u0443\
  \u043F\u0443 \u0434\u043E \u0444\u0430\u0439\u043B\u043E\u0432\u043E\u0457 \u0441\
  \u0438\u0441\u0442\u0435\u043C\u0438, \u0437\u0430\u0437\u0432\u0438\u0447\u0430\
  \u0439 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\
  \u044C\u0441\u044F \u043C\u043E\u0434\u0443\u043B\u044C `fs` \u0434\u043B\u044F\
  \ \u0442\u0430\u043A\u0438\u0445 \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u0439\
  ."
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
weight: 20
---

## Як це зробити:
У Node.js, оскільки JavaScript сам по собі не має прямого доступу до файлової системи, зазвичай використовується модуль `fs` для таких операцій. Ось простий спосіб перевірити, чи існує директорія, використовуючи `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Перевірка наявності директорії
if (fs.existsSync(directoryPath)) {
  console.log('Директорія існує.');
} else {
  console.log('Директорія не існує.');
}
```
**Вивід прикладу:**
```
Директорія існує.
```
Або, для неблокуючого асинхронного підходу, використовуйте `fs.promises` з `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Директорія існує.');
  } catch (error) {
    console.log('Директорія не існує.');
  }
}

checkDirectory('./sample-directory');
```
**Вивід прикладу:**
```
Директорія існує.
```

Для проєктів, які активно використовують операції з файлами та директоріями, пакет `fs-extra`, розширення нативного модуля `fs`, пропонує зручні додаткові методи. Ось як ви можете досягти цього з `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Перевірка наявності директорії
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Директорія існує.' : 'Директорія не існує.'))
  .catch(err => console.error(err));
```
**Вивід прикладу:**
```
Директорія існує.
```

Цей підхід дозволяє мати чистий, читабельний код, який безперервно інтегрується з сучасними практиками JavaScript.
