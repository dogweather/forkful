---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:51.959324-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\
  \u0442\u044C \u0441 YAML \u0432 TypeScript, \u0432\u0430\u043C \u043F\u043E\u043D\
  \u0430\u0434\u043E\u0431\u0438\u0442\u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0430, \u0442\u0430\u043A\u0430\u044F \u043A\u0430\u043A `js-yaml`.\
  \ \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\u0442\u0430\u043D\u043E\
  \u0432\u0438\u0442\u0435 \u0435\u0435."
lastmod: '2024-03-13T22:44:44.626118-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 YAML \u0432 TypeScript, \u0432\u0430\u043C \u043F\u043E\u043D\u0430\
  \u0434\u043E\u0431\u0438\u0442\u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0430, \u0442\u0430\u043A\u0430\u044F \u043A\u0430\u043A `js-yaml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как это сделать:
Чтобы работать с YAML в TypeScript, вам понадобится библиотека, такая как `js-yaml`. Сначала установите ее:

```bash
npm install js-yaml
```

Теперь разберите строку YAML в объект JavaScript:

```typescript
import yaml from 'js-yaml';

const yamlStr = `
name: John Doe
age: 30
`;

try {
  const doc = yaml.load(yamlStr);
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Пример вывода:

```json
{ name: 'John Doe', age: 30 }
```

Чтобы преобразовать объект в строку YAML:

```typescript
import yaml from 'js-yaml';

const obj = { name: 'Jane Doe', age: 25 };

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Пример вывода:

```yaml
name: Jane Doe
age: 25
```

## Погружение в детали
YAML был создан в 2001 году с целью обеспечения удобочитаемости для человека и обмена данными между языками. Он является надмножеством JSON. Альтернативы включают JSON и XML, но минимальный синтаксис YAML часто предпочитают для файлов конфигурации. Работая с YAML в TypeScript, помните, что он не типизирован; будьте осторожны с получаемыми данными, особенно из ненадежных источников, чтобы избежать проблем с безопасностью.

## Смотрите также
- Официальный сайт YAML: http://yaml.org
- Репозиторий `js-yaml` на GitHub: https://github.com/nodeca/js-yaml
- Сравнение YAML и JSON: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
