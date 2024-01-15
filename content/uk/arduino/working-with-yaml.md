---
title:                "Робота з yaml."
html_title:           "Arduino: Робота з yaml."
simple_title:         "Робота з yaml."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви працюєте з Arduino, то вам може знадобитися робота з YAML. YAML - це людино-читаеме структуроване даних, яке може бути корисним для зберігання налаштувань та конфігурацій у вашому проекті Arduino.

## Як

```Arduino
#include <ArduinoYAML.h>

// Створення змінної YAML
YAML settings;

// Додавання ключів та значень до YAML
settings.add("led", "on");
settings.add("temperature", 25.0);

// Збереження YAML у файлі з ім'ям "settings.yaml"
if(settings.save("settings.yaml")){
  Serial.println("Файл settings.yaml успішно збережений.");
}

// Завантаження інформації з YAML файлу
YAML loadedSettings;
if(loadedSettings.load("settings.yaml")){
  Serial.println("Файл settings.yaml успішно завантажений.");
}

// Виведення значень з YAML
Serial.println("Стан світлодіода: " + loadedSettings.get("led").asString());
Serial.println("Температура: " + loadedSettings.get("temperature").asFloat());

// Видалення ключів та їх значень із YAML
loadedSettings.remove("led");
settings.remove("temperature");
```

Вихідний файл `settings.yaml` буде містити:

```yaml
led: on
temperature: 25.0
```

## Вдосконалення

Якщо ви хочете докладніше дослідити роботу з YAML, ви можете використовувати цей пакет у поєднанні з бібліотекою `ArduinoJSON`. Це дозволить вам зберігати більш складні дані, такі як масиви та об'єкти, у вашому файлі YAML.

## Детальніше

Для більш детальної інформації про бібліотеку `ArduinoYAML` та її можливості, ви можете переглянути її офіційну документацію [тут](https://github.com/ivyknob/ArduinoYAML).

## Дивіться також

- [Бібліотека ArduinoJSON](https://arduinojson.org/)
- [Офіційна документація Arduino](https://www.arduino.cc/reference/en/)
- [Основи роботи з YAML](https://yaml.org/start.html)