---
title:                "TypeScript: Читання текстового файлу"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## За що? 
Кожен програміст чи веб-розробник будь-якої мови потенційно зіштовхується з завданням читання текстових файлів. Це неохідний навик для роботи із збереження, відображення та обробки данних у файловій системі. Якщо ви вивчаєте TypeScript або просто цікавитесь програмуванням, то цей пост пропонує корисні вказівки для роботи з текстовими файлами саме у даній мові.

## Як це зробити?
Щоби читати текстовий файл, необхідно використати вбудовану функцію `readFileSync` з Node.js. Нижче наведений приклад коду, де ми читаємо файл `text.txt` та виводимо його вміст на екран:
```TypeScript
import { readFileSync } from 'fs';

const fileContent = readFileSync('text.txt', 'utf-8');
console.log(fileContent);
```
При запуску програми, ви повинні побачити вміст текстового файлу на екрані консолі.

## Глибше занурення
Щоби краще зрозуміти як працює функція `readFileSync`, можна розглянути деякі параметри, які вона має. Перший параметр - це шлях до файлу, який ми хочемо прочитати. Другий параметр вказує на кодування, в якому має бути прочитаний файл. Це дозволяє нам зчитувати файли з різними кодуваннями, такими як UTF-8 чи UTF-16. Щоби отримати краще розуміння роботи з файлами, пропоную детально ознайомитися із [документацією](https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options) по цій функції.

## Дивіться також
- [Інтерактивний курс з TypeScript на сайті Codecademy](https://www.codecademy.com/learn/learn-typescript)
- [Офіційна документація з TypeScript](https://www.typescriptlang.org/docs/home.html)
- [Пост про роботу з файлами в Node.js на сайті Medium](https://medium.com/@vaibhavkhulbe14/file-system-in-node-js-b898b5ece39f)