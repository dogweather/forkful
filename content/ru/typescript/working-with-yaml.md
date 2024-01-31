---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:51.959324-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

YAML - это стандарт сериализации данных, удобный для восприятия человеком. Программисты используют его для файлов конфигурации, обмена данными между языками и не только, поскольку он простой и читаемый.

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
