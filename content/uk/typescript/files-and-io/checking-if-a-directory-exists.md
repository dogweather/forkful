---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:01.958869-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: TypeScript, \u043F\
  \u0440\u0438 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u0456 \u0432 \u0441\
  \u0435\u0440\u0435\u0434\u043E\u0432\u0438\u0449\u0456 Node.js, \u0434\u043E\u0437\
  \u0432\u043E\u043B\u044F\u0454 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u044F\u0442\
  \u0438 \u043D\u0430\u044F\u0432\u043D\u0456\u0441\u0442\u044C \u0434\u0438\u0440\
  \u0435\u043A\u0442\u043E\u0440\u0456\u0457 \u0437\u0430 \u0434\u043E\u043F\u043E\
  \u043C\u043E\u0433\u043E\u044E \u043C\u043E\u0434\u0443\u043B\u044F `fs`, \u044F\
  \u043A\u0438\u0439 \u043D\u0430\u0434\u0430\u0454 \u0444\u0443\u043D\u043A\u0446\
  \u0456\u044E\u2026"
lastmod: '2024-03-13T22:44:48.890431-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u043F\u0440\u0438 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\
  \u043D\u0456 \u0432 \u0441\u0435\u0440\u0435\u0434\u043E\u0432\u0438\u0449\u0456\
  \ Node.js, \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u0435\u0440\u0435\
  \u0432\u0456\u0440\u044F\u0442\u0438 \u043D\u0430\u044F\u0432\u043D\u0456\u0441\u0442\
  \u044C \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\u0457 \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043C\u043E\u0434\u0443\
  \u043B\u044F `fs`, \u044F\u043A\u0438\u0439 \u043D\u0430\u0434\u0430\u0454 \u0444\
  \u0443\u043D\u043A\u0446\u0456\u044E `existsSync()` \u0430\u0431\u043E \u0430\u0441\
  \u0438\u043D\u0445\u0440\u043E\u043D\u043D\u0443 \u0444\u0443\u043D\u043A\u0446\u0456\
  \u044E `access()` \u0432 \u043A\u043E\u043C\u0431\u0456\u043D\u0430\u0446\u0456\u0457\
  \ \u0437 `constants.F_OK`."
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
weight: 20
---

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
