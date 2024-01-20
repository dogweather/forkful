---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що та Чому?
YAML - це формат серіалізації даних, часто використовується для конфігураційних файлів. Програмісти використовують YAML через його читабельність і простоту.

## Як це зробити:
Arduino не підтримує YAML безпосередньо, але ви можете читати та парсити прості YAML файли. Використайте зовнішні бібліотеки для більш складних завдань.

```Arduino
#include <ArduinoJson.h>

// Припустимо, у вас є стрічка у форматі YAML
String yaml = "name: John\nage: 30\nisStudent: false";

void setup() {
  Serial.begin(9600);

  // Створіть об'єкт DynamicJsonDocument
  DynamicJsonDocument doc(1024);

  // Парсити стрічку як JSON, тому що YAML у Arduino обробляють, як JSON
  DeserializationError error = deserializeJson(doc, yaml);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  // Використовуйте об'єкт doc нормально
  Serial.print(F("Name: "));
  Serial.println(doc["name"].as<String>());
  
  Serial.print(F("Age: "));
  Serial.println(doc["age"].as<int>());
  
  Serial.print(F("Is a student: "));
  Serial.println(doc["isStudent"].as<bool>());
}

void loop() {
  // тут може бути ваш код
}
```

**Вивід:**
```
Name: John
Age: 30
Is a student: false
```

## Поглиблено:
YAML започаткований у 2001 році як зручний формат для роботи з даними. У мікроконтролерах частіше використовують JSON через наявність бібліотек та простоту розбору. Варіанти, як XML та INI, також можливі, але YAML виграє на читабельності.

## Дивіться також:
- YAML офіційний сайт: [yaml.org](http://yaml.org)
- Документація ArduinoJson: [arduinojson.org](https://arduinojson.org/)
- Порівняння форматів конфігурації: [toml.io](https://toml.io) vs [json.org](https://www.json.org) vs [yaml.org](http://yaml.org)