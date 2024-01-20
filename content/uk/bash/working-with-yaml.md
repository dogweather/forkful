---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML - це формат даних, зручний для читання людиною. Програмісти використовують його для конфігурацій файлів, відлагодження та передачі даних між сервісами.

## How to:
Встановлення `yq` для роботи з YAML:
```Bash
sudo snap install yq
```
Читання ключа з YAML файлу:
```Bash
echo 'version: "3"' | yq e '.version' -
```
Вивід:
```
"3"
```
Оновлення значення у YAML файлі:
```Bash
echo 'name: MyApp' | yq e '.name = "NewApp"' -
```
Вивід:
```YAML
name: NewApp
```

## Deep Dive
YAML виник у 2001 році як більш читабельна альтернатива до XML та JSON. Альтернативи, такі як TOML або JSON5, також популярні, але в YAML більший фокус на читабельності. Особливістю реалізації роботи з YAML є те, що деякі парсери можуть мати проблеми з великими файлами або особливо складними структурами.

## See Also
- YAML офіційна специфікація: https://yaml.org/spec/1.2/spec.html
- `yq` репозиторій та документація: https://github.com/mikefarah/yq
- YAML vs. JSON: https://phoenixnap.com/kb/yaml-vs-json