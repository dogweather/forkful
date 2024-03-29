---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:38.084527-07:00
description: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u0430\u0439\u043B\u0435 \u0441\
  \ \u0440\u0430\u0441\u0448\u0438\u0440\u0435\u043D\u0438\u0435\u043C `.txt`. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u0432\u0435\u0434\u0435\
  \u043D\u0438\u044F \u043B\u043E\u0433\u043E\u0432, \u043A\u043E\u043D\u0444\u0438\
  \u0433\u0443\u0440\u0430\u0446\u0438\u0438 \u0438\u043B\u0438 \u0441\u043E\u0445\
  \u0440\u0430\u043D\u0435\u043D\u0438\u044F\u2026"
lastmod: '2024-03-13T22:44:44.622422-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u0430\u0439\u043B\u0435 \u0441\
  \ \u0440\u0430\u0441\u0448\u0438\u0440\u0435\u043D\u0438\u0435\u043C `.txt`. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u0432\u0435\u0434\u0435\
  \u043D\u0438\u044F \u043B\u043E\u0433\u043E\u0432, \u043A\u043E\u043D\u0444\u0438\
  \u0433\u0443\u0440\u0430\u0446\u0438\u0438 \u0438\u043B\u0438 \u0441\u043E\u0445\
  \u0440\u0430\u043D\u0435\u043D\u0438\u044F\u2026"
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
---

{{< edit_this_page >}}

## Что и Почему?

Запись текстового файла означает сохранение данных в файле с расширением `.txt`. Программисты делают это для ведения логов, конфигурации или сохранения простых данных без необходимости использования базы данных.

## Как это сделать:

TypeScript, будучи надмножеством JavaScript, не имеет собственного модуля файловой системы, но может использовать Node.js для этой задачи. Убедитесь, что у вас установлен Node.js, и тогда приступим:

```typescript
// Импортирование модуля 'fs' для взаимодействия с файловой системой
import { writeFile } from 'fs';

// Контент, который вы хотите записать
const content = 'Привет, мир!';

// Функция для записи контента в файл
const writeTextToFile = (filePath: string, content: string): void => {
  writeFile(filePath, content, (err) => {
    if (err) {
      console.error('Ошибка при записи файла:', err);
    } else {
      console.log('Файл успешно записан');
    }
  });
};

// Использование функции для записи в 'output.txt'
writeTextToFile('./output.txt', content);
```

Пример вывода:
```
Файл успешно записан
```

## Углубляемся

Исторически, запись в текстовые файлы столь же стара, как и само вычислительное дело, используется для хранения данных или коммуникации между программами. До того, как базы данных стали распространенны, обычными были плоские файлы. Сейчас базы данных в значительной степени переняли эту роль, но текстовые файлы по-прежнему имеют важное значение за счет их простоты.

Альтернативы модулю 'fs' Node.js включают:

- Новый 'fs/promises' для функций, основанных на обещаниях.
- Использование 'fs-extra' для удобства методов.
- Модуль 'stream' для работы с большими файлами.

Показанный выше метод 'writeFile' хорошо работает для файлов небольшого и среднего размера. Для больших файлов или потоков данных вы можете захотеть использовать потоки, чтобы избежать загрузки всего в память.

## См. также

- API файловой системы Node.js: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Официальная страница TypeScript: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- Библиотека 'fs-extra': [https://github.com/jprichardson/node-fs-extra](https://github.com/jprichardson/node-fs-extra)
- Документация MDN по потокам: [https://developer.mozilla.org/ru/docs/Web/API/Streams_API](https://developer.mozilla.org/ru/docs/Web/API/Streams_API)
