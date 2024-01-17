---
title:                "Робота з yaml"
html_title:           "Python: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що та чому?
Робота з YAML - це процес створення і редагування структурованих даних, який використовують програмісти для збереження та обміну конфігураційною інформацією. YAML - це простий формат для зберігання даних у вигляді тексту, який може бути легко зрозумілим і змінюваним людиною.

## Як?
Використання YAML у Python - це просто. Спочатку потрібно імпортувати модуль PyYAML, після чого можна створювати та редагувати файли YAML з допомогою вбудованих функцій. Наприклад:

```Python
import yaml 

# Створення нового YAML файлу
data = {'key': 'value'}
with open('file.yaml', 'w') as file:
    yaml.dump(data, file)

# Читання інформації з YAML файлу
with open('file.yaml') as file:
    data = yaml.load(file, Loader=yaml.FullLoader)
    print(data) # {'key': 'value'}
```

## Глибоке занурення
YAML було створено у 2001 році як заміна для складного формату XML. Він став широко використовуваним для збереження конфігураційних файлів у програмуванні, оскільки він більш зрозумілий та зручний для редагування людиною. YAML також має альтернативи, такі як JSON та Toml, але YAML вже зарекомендував себе як потужний та зручний для використання формат даних.

## Також дивіться
- [Документація PyYAML](https://pyyaml.org/)
- [Офіційна сторінка YAML](https://yaml.org/)
- [Репозиторій YAML на GitHub](https://github.com/yaml/yaml)