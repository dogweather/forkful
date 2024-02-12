---
title:                "Перевірка наявності директорії"
aliases:
- /uk/javascript/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:00.633928-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Перевірка наявності директорії в JavaScript є невід'ємною для завдань маніпулювання файлами, дозволяючи скриптам підтверджувати наявність директорії перед читанням з неї або записом у неї. Ця операція запобігає помилкам і забезпечує плавнішу виконання програми, особливо в додатках, які динамічно обробляють файли або директорії на основі введення користувача або зовнішніх даних.

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
