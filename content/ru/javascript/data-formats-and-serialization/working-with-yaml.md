---
title:                "Работа с YAML"
aliases: - /ru/javascript/working-with-yaml.md
date:                  2024-01-29T00:05:32.983706-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
YAML — это "YAML Ain't Markup Language" (YAML — это не язык разметки), который представляет собой стандарт сериализации данных, удобный для восприятия человеком, для всех языков программирования. Программисты работают с YAML, потому что его легко читать и писать, его часто используют для файлов конфигурации и обмена данными между языками или сервисами.

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
