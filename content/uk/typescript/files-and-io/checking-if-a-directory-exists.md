---
title:                "Перевірка наявності директорії"
aliases:
- /uk/typescript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:09:01.958869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Перевірка наявності директорії у TypeScript є суттєвою для завдань управління файлами, наприклад, для читання з файлів або запису даних у файли, забезпечуючи виконання операцій лише над існуючими директоріями. Ця операція критично важлива для уникнення помилок, що виникають при спробі доступу або маніпулювання неіснуючими директоріями.

## Як робити:

TypeScript, при виконанні в середовищі Node.js, дозволяє перевіряти наявність директорії за допомогою модуля `fs`, який надає функцію `existsSync()` або асинхронну функцію `access()` в комбінації з `constants.F_OK`.

### Використання `fs.existsSync()`:

```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Директорія існує.');
} else {
  console.log('Директорії не існує.');
}
```

### Використання `fs.access()` з `fs.constants.F_OK`:

```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Директорії не існує.');
    return;
  }
  console.log('Директорія існує.');
});
```

**Приклад виводу** для обох методів, виходячи з припущення, що директорія існує:
```
Директорія існує.
```

А якщо ні:
```
Директорії не існує.
```

### Використання сторонньої бібліотеки - `fs-extra`:

`fs-extra` - це популярна стороння бібліотека, яка покращує вбудований модуль `fs` і надає більш зручні функції.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Директорія існує: ${exists}`);
});
```

**Приклад виводу** коли директорія існує:
```
Директорія існує: true
```

А якщо ні:
```
Директорія існує: false
```
