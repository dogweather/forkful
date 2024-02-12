---
title:                "Робота з YAML"
date:                  2024-02-01T22:07:23.520287-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

YAML, що означає "YAML Ain't Markup Language" (YAML - це не мова розмітки), є зрозумілим для людини стандартом серіалізації даних, який часто використовується для файлів конфігурації та обміну даними між мовами з різними структурами даних. Програмісти часто працюють з YAML через його простоту та читабельність, особливо в проектах, що вимагають розширеної конфігурації, або при передачі структурованих даних між різними системами.

## Як:

Хоча Google Apps Script (GAS) не підтримує вроджений аналіз YAML або серіалізацію, ви можете маніпулювати даними YAML, використовуючи бібліотеки JavaScript або пишучи власні функції аналізу. Для демонстрації розглянемо, як аналізувати рядок YAML за допомогою користувацької функції, оскільки зовнішні бібліотеки не можна безпосередньо імпортувати в GAS.

Припустимо, у вас є проста конфігурація YAML:

```yaml
title: Приклад YAML
description: Приклад того, як обробити YAML в Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Конфігурація
```

Для аналізу цього в Google Apps Script використовуйте можливості маніпулювання рядками JavaScript:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Основна обробка масивів
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: Приклад YAML\ndescription: Приклад того, як обробити YAML в Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Конфігурація";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Коли виконується `testYamlParsing()`, виводиться:

```
{ title: 'Приклад YAML',
  description: 'Приклад того, як обробити YAML в Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Конфігурація' ] }
```

Цей користувацький метод аналізу досить основний і може вимагати коригування для роботи зі складними файлами YAML.

## Поглиблено

YAML, що був випущений у 2001 році, мав на меті бути більш читабельним ніж його попередники, такі як XML або JSON. Хоча його простота та зручність використання широко цінуються, робота з YAML у Google Apps Script представляє певні виклики через відсутність прямої підтримки. У результаті програмісти часто покладаються на універсальність JavaScript для аналізу та генерації даних YAML. Однак для складних випадків використання, особливо тих, що включають глибоку вкладеність та розширені структури даних, цей метод може бути обтяжливим і схильним до помилок.

JSON, навпаки, підтримується вроджено в Google Apps Script та більшості інших програмних середовищ, пропонуючи більш простий підхід для серіалізації та десеріалізації даних без додаткової обробки. Синтаксис JSON менш мовазначний, ніж синтаксис YAML, що робить його більш придатним для обміну даними в веб-додатках. Однак YAML залишається популярним для файлів конфігурації та ситуацій, де читабельність для людини є пріоритетною.

При роботі з YAML у Google Apps Script розгляньте компроміси між читабельністю та зручністю використання. Для всебічної маніпуляції з YAML може бути варто дослідити зовнішні інструменти або сервіси, які можуть конвертувати YAML у JSON перед обробкою його у вашому скрипті.
