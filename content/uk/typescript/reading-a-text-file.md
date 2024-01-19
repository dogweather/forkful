---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Читання текстового файлу - це процес, коли програма зчитує дані з файлу у форматі тексту. Програмісти роблять це, щоб маніпулювати збереженими даними та використовувати їх у чітко визначених цілях.

## Як це зробити:
Ось базовий приклад, як читати текстовий файл у TypeScript:

```TypeScript
import { readFileSync } from 'fs';

try {
  let data = readFileSync('path/to/your/file.txt', 'utf8');
  console.log(data);
} catch (err) {
  console.error(err);
}
```
При виконанні цього коду в консолі відобразиться вміст файлу 'file.txt'. Ця програма використовує метод `readFileSync` з модуля 'fs' для синхронного читання файлу.

## Поглиблений аналіз
1. **Історичний контекст:** У минулому, програмісти часто використовували нативні функції мови для читання текстових файлів. Однак, з часом з'явились спеціалізовані бібліотеки, як-от 'fs' у Node.js, які навіть більше оптимізували цей процес.
2. **Альтернативи:** Для асинхронного читання текстових файлів можна використати `fs.readFile`. Є також бібліотеки, як-от 'fs-extra', які надають додаткові функції та опції.
3. **Деталі реалізації:** Коли ви викликаєте `readFileSync`, він блокує виконання вашої програми, поки файл не буде повністю зчитаний. Це може бути проблематичним для великих файлів або для програм, які повинні залишатися реактивними.

## Дивись також
1. Документація Node.js 'fs' модуля: [тут](https://nodejs.org/api/fs.html)
2. Бібліотека 'fs-extra' на npm: [тут](https://www.npmjs.com/package/fs-extra)
3. StackOverflow обговорення про `readFileSync` vs `readFile`: [тут](https://stackoverflow.com/questions/17604866/difference-between-readfile-vs-readfilesync)
4. Курс "The Complete Node.js Developer Course" на Udemy: [тут](https://www.udemy.com/course/the-complete-nodejs-developer-course-2/)