---
title:    "TypeScript: Читання текстового файлу"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Чому

Кожен веб-розробник повинен уміти зчитувати текстові файли, це важлива навичка для роботи з даними у веб-додатках. Дізнайтеся, як за допомогою TypeScript прочитати текстовий файл і отримати доступ до його вмісту.

## Як це зробити

Начати роботу з текстовими файлами в TypeScript досить просто. Спочатку створіть новий файл з розширенням .txt, в який введіть будь-який текст, наприклад:

```TypeScript
Hello World!
This is a sample text file.
```

Потім відкрийте цей файл у вашому TypeScript проекті за допомогою вбудованого модуля `fs`:

```TypeScript
import * as fs from 'fs'; // Імпорт модуля fs

fs.readFile('sample.txt', (err, data) => { // Зчитуємо файл "sample.txt"
  if (err) {
    console.error(err); // Обробляємо помилку, якщо вона виникла
  } else {
    console.log(data.toString()); // Виводимо вміст файлу у консоль
  }
});
```

Результатом буде виведення вмісту зчитаного файлу у консоль:

```
Hello World!
This is a sample text file.
```

## Глибоке погруження

На попередньому кроці ми використали метод `readFile()` модуля `fs`, щоб прочитати вміст файлу. Але що робити, якщо нам потрібно зчитати вміст текстового файлу по рядках? Для цього використовуйте метод `readline()`:

```TypeScript
import * as fs from 'fs';
import * as readline from 'readline'; // Імпорт модуля readline

const fileStream = fs.createReadStream('sample.txt'); // Створюємо потік для читання файлу

const rl = readline.createInterface({ // Створюємо інтерфейс для читання потоку
  input: fileStream,
  crlfDelay: Infinity // Використовуємо кінець рядка за замовчуванням для текстових файлів
});

rl.on('line', (line) => { // Обробляємо кожен рядок файлу окремо
  console.log(`Line: ${line}`);
})
```

Результатом буде виведення кожного рядка зчитаного файлу окремо у консоль:

```
Line: Hello World!
Line: This is a sample text file.
```

## Дивись також

- [Офіційна документація TypeScript](https://www.typescriptlang.org/docs/)
- [Модуль fs у Node.js](https://nodejs.org/api/fs.html)
- [Модуль readline у Node.js](https://nodejs.org/api/readline.html)