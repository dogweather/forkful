---
title:                "Початок нового проєкту"
html_title:           "Elm: Початок нового проєкту"
simple_title:         "Початок нового проєкту"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що і чому?

Початок нового проекту - це перше завдання програміста для втілення ідеї. Нам це потрібно, щоб налагодити базову структуру, задати тон і напрямок роботи.

## Як до цього братися:

Розглянемо базовий приклад програми для Arduino:

```Arduino
void setup() {
  pinMode(13, OUTPUT); // Налаштування 13-го піна на вивід
}

void loop() {
  digitalWrite(13, HIGH);  // Включити LED
  delay(1000);             // Чекати одну секунду
  digitalWrite(13, LOW);   // Вимкнути LED
  delay(1000);             // Чекати ще одну секунду
}
```
Це програма мигає світлодіодом на 13-му піні Arduino UNO періодично кожну секунду.

## Глибше:

- **Історичний контекст**: Arduino - це відкрита платформа, розроблена у 2005 році, призначена для створення електронних пристроїв, що можуть спілкуватися з реальним світом через датчики та приводи.
- **Альтернативи**: Є інші плати, такі як Raspberry Pi, ESP8266, STM32, які можуть бути використані як альтернативи Arduino.
- **Деталі впровадження**: Початок нового проекту Arduino вимагає розуміння основних стадій впровадження: складання схеми, написання коду і завантаження його на плату, тестування та відладка.

## Дивіться також:

- [Arduino офіційний сайт](https://www.arduino.cc/)
- [Введення в Arduino для початківців](https://www.makeuseof.com/tag/arduino-beginners-guide-intro/)
- [Огляд початоку нового проекту Arduino](https://www.youtube.com/watch?v=fCxzA9_kg6s)
- [Arduino Tutorial for Beginners](https://create.arduino.cc/projecthub/ROBINTHOMAS/programming-arduino-with-c-3a2860)  
- [Відео-курси Arduino на українській мові](https://www.youtube.com/playlist?list=PL3yE_I_mq0oDxlTjPhI5GLgVqAQNIyQ3Q)