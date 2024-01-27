---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
YAML - це легко для людського ока читаємий формат даних, часто використовується для конфігурації програм. Програмісти працюють з YAML, бо він інтуїтивно зрозумілий та легко інтегрується з різними мовами програмування.

## Як це робити:
```TypeScript
import * as yaml from 'js-yaml';
import { readFileSync, writeFileSync } from 'fs';

// Читання YAML файлу
const doc = yaml.load(readFileSync('./config.yaml', 'utf8'));
console.log(doc);

// Конвертація об'єкта в YAML і запис у файл
const newData = { name: 'Василь', age: 34 };
const newYaml = yaml.dump(newData);
writeFileSync('./new-config.yaml', newYaml, 'utf8');
```
Вивід:
```yaml
name: 'Василь'
age: 34
```

## Поглиблені знання:
YAML, або "YAML Ain't Markup Language", було створено в 2001 році. Хоча JSON та XML теж популярні для конфігураційних файлів, YAML часто вибирається за його читабельність. Основною проблемою при роботі з YAML є його чутливість до пробілів, що може призводити до помилок. Бібліотеки, як js-yaml для JavaScript (і TypeScript), допомагають з парсингом та генерацією YAML.

## Дивіться також:
- YAML офіційна сторінка: https://yaml.org
- js-yaml GitHub репозиторій: https://github.com/nodeca/js-yaml
- Хороші практики YAML: https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html
- YAML vs JSON: https://phoenixnap.com/kb/yaml-vs-json-vs-xml
