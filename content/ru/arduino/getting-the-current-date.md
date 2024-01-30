---
title:                "Получение текущей даты"
date:                  2024-01-28T23:58:50.255500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Получение текущей даты на Arduino означает запрос к модулю реального времени (RTC) или интернет-сервису времени, чтобы узнать нынешнюю дату. Зачем это делать? Ведение журнала событий, добавление временных отметок к данным или планирование действий — знание даты может быть критически важным для этих задач.

## Как это сделать:

Давайте сделаем наш Arduino умён в плане даты. Мы будем использовать модуль RTC, такой как DS3231, который является точным и имеет резервный аккумулятор.

```arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Не удалось найти RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC потеряло питание, давайте установим время!");
    // следующая строка устанавливает в RTC дату и время компиляции этого скетча
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  
  delay(3000); // ждём 3 секунды перед обновлением даты
}
```

Пример вывода:
```
2023/4/5
```

## Подробнее:
Исторический контекст? Ранние компьютеры не нуждались в знании даты. Это стало важным только с появлением систем ведения журналов и многопользовательских систем. В наши дни это уже просто необходимость.

Альтернативы RTC включают использование Протокола сетевого времени (NTP) при подключении к интернету или модулей GPS, которые предоставляют точные сведения о времени и дате.

Детали реализации имеют значение. Не все модули RTC созданы равными. Некоторые, как DS1307, менее точны и могут больше отклоняться со временем. Библиотеки типа `RTClib.h` абстрагируют различия между модулями, облегчая вашу жизнь.

Использование NTP через WiFi требует другого подхода. Вам понадобится ESP8266 или ESP32 с доступом в интернет и включение библиотек типа `WiFi.h` и `NTPClient.h`. Схема кодирования меняется — вы делаете периодические запросы к серверу времени и анализируете результаты для получения даты.

## Смотрите также:
- [Библиотека RTClib](https://github.com/adafruit/RTClib): Библиотека, облегчающая взаимодействие с модулями RTC.
- [Технические характеристики DS3231](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf): Всё о модуле RTC DS3231.
- [Библиотека NTPClient](https://github.com/arduino-libraries/NTPClient): Для получения времени через интернет.
- [Время и дата на Arduino без RTC](https://create.arduino.cc/projecthub/Arnov_Sharma_makes/time-and-date-on-arduino-without-a-rtc-module-c7d2d6): Альтернативные методы, если у вас нет RTC.