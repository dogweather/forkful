---
title:                "Arduino: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Багато людей захоплюються програмуванням на Arduino і хочуть вивчити нові технології та інструменти. YAML (YAML Ain't Markup Language) - це сучасний формат для представлення структурованих даних, що використовується для збереження і обміну даними. Вивчення YAML буде корисним для програмістів, які хочуть зрозуміти, як ефективно працювати зі структурованими даними та легко спрямовувати їх у свої програми.

## Як

Існує багато бібліотек для роботи з YAML у середовищі Arduino, наприклад, ArduinoYaml (https://github.com/ttezel/ArduinoYaml), YamlForArduino (https://github.com/moutend/YamlForArduino) та інші. Розглянемо декілька прикладів використання YAML.

```Arduino
#include <YAML.h>

void setup(){
  Serial.begin(9600);
  DynamicYamlDocument doc;
  doc["name"] = "Василь";
  doc["age"] = 28;
  doc["city"] = "Київ";
  doc["hobbies"] = {"подорожі", "комп'ютерні ігри", "спорт"};
  Serial.println(doc);
}

void loop(){
  // код для циклічного виконання
}
```

Цей код виведе на монітор порта «Serial» такий результат:

```
name: Василь
age: 28
city: Київ
hobbies:
- подорожі
- комп'ютерні ігри
- спорт
```

Також, за допомогою бібліотеки "YAML for Arduino", можна зчитати дані з файлу YAML:

```Arduino
#include <YAML.h>

void setup(){
  Serial.begin(9600);
  File yamlFile = SD.open("config.yaml");
  StaticJsonDocument<200> doc;
  readYamlFile(yamlFile, doc);
  int maxTemp = doc["max_temp"];
  Serial.println(maxTemp);
}

void loop(){
  // код для циклічного виконання
}
```

У файлі "config.yaml" має бути наступний вміст:

```
max_temp: 35
```

Цей код виведе на монітор порта «Serial» значення "35".

## Глибока поглиблення

Як було вже сказано, YAML є форматом для представлення структурованих даних у зручній для читання формі. Також можна використовувати коментарі в YAML-файлах, що дозволяє зберігати додаткову документацію для даних. Крім того, YAML підтримує різні типи даних, такі як рядки, числа, масиви та об'єкти, тому дуже зручно використовувати його для збереження конфігураційних даних для ваших проектів на Arduino.

## Дивіться також

1. Документація по YAML (українською): https://yaml.org.ua/
2. Офіційна документація по бібліотеці ArduinoYaml: https://github.com/ttezel/ArduinoYaml
3. Приклади використання бібліотек