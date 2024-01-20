---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?
Порівняння двох дат – це процес встановлення того, яка дата була раніше, пізніше чи чи обидві дати однакові. Програмісти роблять це, щоб виконати дії, які пов'язані з часовими рамками, такі як слідкування за термінами та двигунами часу.

## Як це робити:
```Arduino
#include <TimeLib.h> // Використовуємо TimeLib для роботи з датами

time_t t1 = tmConvert(2022, 8, 12, 14, 15, 0); // Определяємо першу дату
time_t t2 = tmConvert(2023, 2, 28, 8, 30, 0); // Определяємо другу дату

if(t1 > t2){ // Перевірка, яка дата більша
  Serial.println("Перша дата більша за другу");
} else if (t1 < t2){
  Serial.println("Друга дата більша за першу");
} else {
  Serial.println("Дати однакові");
}

// Функція для конвертації дати і часу в UNIX формат
time_t tmConvert(int YYYY, byte MM, byte DD, byte hh, byte mm, byte ss) {
  tmElements_t tm;
  tm.Year = CalendarYrToTm(YYYY);
  tm.Month = MM;
  tm.Day = DD;
  tm.Hour = hh;
  tm.Minute = mm;
  tm.Second = ss;
  return makeTime(tm);
}
```
Вихідні дані:
```
"Друга дата більша за першу"
```

## Глибший взгляд
Частина цієї задачі полягає в поніманні "часу Unix", або "позначок часу Unix": це кількість секунд, що минуло від початку епохи Unix (00:00:00 UTC 1 січня 1970 року). Історично, сформулований таким чином "час Unix" був найбільш надійним способом подолати питання порівняння часу, незважаючи на зміни в часових поясах або перехід на літній час. 

Альтернативою може бути використання вбудованого бібліотеки Arduino, яка називається DS3231, що працює з апаратними модулями часу реальної дати (RTC). Це йде разом з методами для порівняння дат, але вимагає фізичної RTC пристрій у вашій схемі.

## Дивіться також
1. Бібліотека TimeLib Arduino - https://www.arduinolibraries.info/libraries/time
2. Arduino DS3231 бібліотека - https://www.arduinolibraries.info/libraries/ds3231
3. Unix час - https://uk.wikipedia.org/wiki/Unix-час