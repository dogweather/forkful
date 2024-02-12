---
title:                "Написання текстового файлу"
aliases:
- uk/javascript/writing-a-text-file.md
date:                  2024-02-03T19:28:51.797115-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написання текстового файлу"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Запис текстового файлу в JavaScript часто пов’язаний зі створенням та збереженням даних у простому, зрозумілому форматі для ведення журналів, експорту даних користувача або налаштувань. Ця функціональність є важливою для додатків, яким потрібно зберігати дані поза часом життя процесу додатку, надаючи спосіб зберігання та подальшого отримання або спільного використання інформації.

## Як:
У середовищі Node.js ви можете використовувати вбудований модуль `fs` (Файлова Система) для запису текстових файлів. Цей приклад демонструє асинхронний запис тексту в файл:

```javascript
const fs = require('fs');

const data = 'Привіт, Світ! Це текст, який буде записаний у файл.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Файл було записано.');
});
```

Приклад виводу:
```
Файл було записано.
```

Для синхронного запису файлів використовуйте `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('Файл було записано.');
} catch (error) {
  console.error('Помилка запису файлу:', error);
}
```

У сучасних веб-браузерах API доступу до файлової системи надає можливість читати та записувати файли. Однак, його використання підпорядковується дозволам користувача. Ось як створити та записати у файл:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Привіт, Світ! Це запис файлу у браузері.');
  await writable.close();
}
```

Для більш складних сценаріїв або при роботі з великими файлами ви можете вибрати сторонні бібліотеки, такі як `FileSaver.js` для браузерів:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Привіт, Світ! Це текст від FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Пам'ятайте, запис файлів на клієнтській стороні (у браузерах) обмежений з міркувань безпеки, і будь-яка операція, яка потребує збереження на локальний диск користувача, зазвичай вимагатиме їхнього явного дозволу.
