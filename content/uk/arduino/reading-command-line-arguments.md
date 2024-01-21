---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:55:52.064145-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це таке & Чому?
Читання аргументів командного рядка дозволяє забезпечити гнучкість і контроль над поведінкою програми через параметри запуску. Програмісти це роблять, щоб зробити свої додатки налаштовуваними та зручними для конкретних сценаріїв використання.

## Як це зробити:
Arduino не підтримує аргументи командного рядка традиційним способом, як це робить ПК-софт, але ви можете емулювати цю поведінку, читаючи дані з серійного порта.

```Arduino
void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // чекаємо, поки серійний порт не буде активний
  }
  Serial.println("Введіть аргументи, розділені пробілами:");
}

void loop() {
  // Чекаємо нових даних від користувача 
  if (Serial.available() > 0) {
    // Читаємо рядок
    String input = Serial.readStringUntil('\n');
    // Розділяємо рядок на аргументи
    int firstSpaceIndex = input.indexOf(' ');
    while (firstSpaceIndex != -1) {
      String argument = input.substring(0, firstSpaceIndex);
      Serial.print("Аргумент: ");
      Serial.println(argument);
      input = input.substring(firstSpaceIndex + 1);
      firstSpaceIndex = input.indexOf(' ');
    }
    // Не забудьте останній аргумент
    Serial.print("Аргумент: ");
    Serial.println(input);
  }
}
```
Очікуваний вивід при введенні "arg1 arg2 arg3":
```
Аргумент: arg1
Аргумент: arg2
Аргумент: arg3
```

## Глибше занурення
На відміну від систем, які дозволяють безпосередньо передавати аргументи в командний рядок, Arduino працює із вбудованим мікроконтролером, де немає традиційної ОС. Тому аргументи можна передавати лише через периферійні інтерфейси, такі як серійний порт. Цей підхід імітує читання аргументів командного рядка, але вимагає спілкування з Ардуіно за допомогою ПК чи іншого пристрою.

Програмісти часто використовують серійний монітор в Arduino IDE для введення команд чи параметрів. Це дає можливість легко взаємодіяти з мікроконтролером. 

Інші підходи включають читання з SD-карти чи Інтернету через Wi-Fi чи Ethernet модулі.

## Посилання для ознайомлення
- [Arduino Serial Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino SoftwareSerial Library](https://www.arduino.cc/en/Reference/SoftwareSerial)
- [Working with Strings in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)

Ці ресурси допоможуть розширити ваше розуміння серійної комунікації та роботи з рядками в Arduino.