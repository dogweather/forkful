---
title:                "Створення тимчасового файлу."
html_title:           "TypeScript: Створення тимчасового файлу."
simple_title:         "Створення тимчасового файлу."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасових файлів допомагає уникнути загромадження оперативної пам'яті під час виконання програми, а також забезпечує безпечне та організоване збереження даних.

## Як

Найпростішим способом створення тимчасового файлу є використання вбудованої функції `tmpfile ()` в TypeScript. Ця функція створює тимчасовий файл у потрібному розташуванні тимчасової директорії та повертає об'єкт `File` для подальшої роботи з файлом. 

```TypeScript
let tmpFile = tmpfile();
console.log(tmpFile.name); // назва створеного файлу
console.log(tmpFile.size); // розмір файлу
```

У разі потреби, можна визначити минулий шлях до тимчасової директорії та вручну створити тимчасовий файл за допомогою `path` та `fs` модулів. 

```TypeScript
const tempDir = os.tmpdir();
const fileName = 'tempFile';
const tempFile = path.join(tempDir, fileName);
fs.closeSync(fs.openSync(tempFile, 'w')); // створення тимчасового файлу в потрібній директорії
```

## Глибоке дослідження

Коли створюється тимчасовий файл, він зберігається в тимчасовій директорії комп'ютера, що часто знаходиться в оперативній пам'яті. Це означає, що доступ до файлу відбувається швидше, ніж при доступі до зовнішнього файлу на жорсткому диску. 

Крім того, після завершення роботи з тимчасовим файлом, він автоматично видаляється з оперативної пам'яті, звільняючи місце для інших процесів. Це дозволяє оптимізувати роботу програми та запобігає можливим конфліктам з іншими файлами.

## Дивись також

- [Документація TypeScript для `tmpfile()`](https://www.typescriptlang.org/docs/handbook/file-system-and-d-ts.html#-tmpfile)
- [Офіційна документація Node.js для роботи з тимчасовими файлами](https://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options)
- [Посібник по роботі з тимчасовими файлами в TypeScript](https://blog.logrocket.com/temporary-file-creation-with-node-js-and-typescript/)