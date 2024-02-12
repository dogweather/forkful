---
title:                "Написання текстового файлу"
aliases:
- uk/typescript/writing-a-text-file.md
date:                  2024-02-03T19:30:06.837924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написання текстового файлу"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Створення текстового файлу в TypeScript є критично важливим навиком для збереження даних, конфігурацій або генерації журналів. Програмісти часто виконують це завдання, щоб зберігати та маніпулювати даними поза пам'яті програми з різних причин, таких як аналіз даних, звітування або просто зберігання налаштувань користувача між сесіями.

## Як:
TypeScript сам по собі не виконує операції із файлами безпосередньо, оскільки він компілюється в JavaScript, який традиційно виконується у браузері з обмеженим доступом до файлової системи. Однак, коли використовується в середовищі Node.js, модуль `fs` (File System) надає функціональність для запису файлів.

### Використання модуля fs Node.js
Спочатку переконайтеся, що ви працюєте в середовищі Node.js. Потім використовуйте модуль `fs` для запису текстових файлів. Ось простий приклад:

```typescript
import * as fs from 'fs';

const data = 'Привіт, світе!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('Файл було збережено!');
});
```

Це асинхронно записує "Привіт, світе!" у `message.txt`. Якщо файл не існує, Node.js створює його; якщо існує, Node.js перезаписує його.

Для синхронного запису файлу використовуйте `writeFileSync`:

```typescript
import * as fs from 'fs';

const data = 'Привіт ще раз, світе!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('Файл було збережено!');
} catch (err) {
    console.error(err);
}
```

### Використання популярних сторонніх бібліотек
Хоча рідний модуль `fs` є потужним, деякі розробники вважають за краще використовувати сторонні бібліотеки для додаткової зручності та функціональності. `fs-extra` є популярним вибором, який розширює `fs` та спрощує операції з файлами.

Спочатку вам потрібно встановити `fs-extra`:

```
npm install fs-extra
```

Потім ви можете використовувати його у своєму TypeScript файлі для запису текстового вмісту:

```typescript
import * as fs from 'fs-extra';

const data = 'Це fs-extra!';
const filePath = './extraMessage.txt';

// Використання async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('Файл було збережено за допомогою fs-extra!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

Цей фрагмент коду робить те ж саме, що й попередні приклади з `fs`, але використовує бібліотеку `fs-extra`, пропонуючи більш чистий синтаксис для роботи з промісами.
