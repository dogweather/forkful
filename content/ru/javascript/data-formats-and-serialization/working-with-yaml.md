---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:32.983706-07:00
description: "YAML \u2014 \u044D\u0442\u043E \"YAML Ain't Markup Language\" (YAML\
  \ \u2014 \u044D\u0442\u043E \u043D\u0435 \u044F\u0437\u044B\u043A \u0440\u0430\u0437\
  \u043C\u0435\u0442\u043A\u0438), \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043F\
  \u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442 \u0441\u043E\u0431\
  \u043E\u0439 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442 \u0441\u0435\u0440\
  \u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\u0430\u043D\u043D\u044B\
  \u0445, \u0443\u0434\u043E\u0431\u043D\u044B\u0439 \u0434\u043B\u044F \u0432\u043E\
  \u0441\u043F\u0440\u0438\u044F\u0442\u0438\u044F\u2026"
lastmod: '2024-03-13T22:44:45.796900-06:00'
model: gpt-4-0125-preview
summary: "YAML \u2014 \u044D\u0442\u043E \"YAML Ain't Markup Language\" (YAML \u2014\
  \ \u044D\u0442\u043E \u043D\u0435 \u044F\u0437\u044B\u043A \u0440\u0430\u0437\u043C\
  \u0435\u0442\u043A\u0438), \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043F\u0440\
  \u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442 \u0441\u043E\u0431\u043E\
  \u0439 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442 \u0441\u0435\u0440\u0438\
  \u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445\
  , \u0443\u0434\u043E\u0431\u043D\u044B\u0439 \u0434\u043B\u044F \u0432\u043E\u0441\
  \u043F\u0440\u0438\u044F\u0442\u0438\u044F \u0447\u0435\u043B\u043E\u0432\u0435\u043A\
  \u043E\u043C, \u0434\u043B\u044F \u0432\u0441\u0435\u0445 \u044F\u0437\u044B\u043A\
  \u043E\u0432 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0440\u043E\u0432\
  \u0430\u043D\u0438\u044F."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как это сделать:
Мы будем использовать популярную библиотеку `js-yaml` для разбора YAML в объекты JavaScript и строкового представления объектов JavaScript в YAML.

1. Сначала установите библиотеку:

```bash
npm install js-yaml
```

2. Разбор YAML в JavaScript:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

try {
  const doc = yaml.load(fs.readFileSync('config.yml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Пример вывода, если `config.yml` содержит:

```yaml
version: 1
services:
  - webapp
  - database
```

Может выглядеть так:

```javascript
{ version: 1, services: [ 'webapp', 'database' ] }
```

3. Преобразование JavaScript в YAML:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

let data = {
  title: "Пример YAML",
  description: "YAML это просто"
};

try {
  const ymlText = yaml.dump(data);
  fs.writeFileSync('example.yml', ymlText, 'utf8');
} catch (e) {
  console.error(e);
}
```

Это создаст файл `example.yml` со следующим содержанием:

```yaml
title: Пример YAML
description: 'YAML это просто'
```

## Глубокое погружение
YAML был создан в 2001 году, разработан так, чтобы его было легко читать для людей и удобно писать вручную. JSON и XML являются альтернативами, но они не так просты для восприятия человеком. Простота YAML может привести к проблемам безопасности, если он не реализован правильно, например, необходимо отключить `!!python/object/apply` для предотвращения выполнения произвольного кода. Библиотеки, такие как `js-yaml`, предлагают опции для настройки разбора и строкового представления YAML для добавления безопасности и функциональности.

## Смотрите также
- Спецификация YAML 1.2: https://yaml.org/spec/1.2/spec.html
- GitHub репозиторий js-yaml: https://github.com/nodeca/js-yaml
- Статья о YAML в Википедии для дополнительной информации: https://en.wikipedia.org/wiki/YAML
- Сравнение JSON и YAML: https://phoenixnap.com/kb/yaml-vs-json
