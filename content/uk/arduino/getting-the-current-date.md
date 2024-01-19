---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?

Отримання поточної дати - це процес, який дозволяє програмістам визначати та використовувати сьогоднішню дату у своїх програмах. Це корисно для відслідкування подій, створення журналів подій та часових міток.

## Як це зробити:

Для отримання поточної дати використовуємо модуль часу DS3231. Це схема Arduino та приклад виводу:

```Arduino
#include <Wire.h>
#include <DS3231.h>

DS3231 clock;
RTCDateTime dt;

void setup() {
  Serial.begin(9600);
  clock.begin();

  dt = clock.getDateTime();
  Serial.print("Current date: ");
  Serial.print(dt.year);       Serial.print("-");
  Serial.print(dt.month);      Serial.print("-");
  Serial.println(dt.day);
}

void loop() {
  
}
```

При виконанні цього коду, виводом на ваш серійний монітор було б:

```Arduino
Current date: 2022-11-24
```

## Пірнання у глибину

Початково програми Arduino використовують дату і час RTC (реальний час), що інтегровані на комп'ютері. Проте, Arduino Uno не має вбудованого RTC, тому ми використовуємо модуль DS3231. 

Альтернативою є інтернет-час, який може надавати дату і час, але це вимагає підключення до інтернету.

Особливості виконання: Після отримання значення дати і часу, ви можете використовувати ці дані для вирішення ряду задач, включаючи керування процесами, що залежать від часу.

## Дивіться також

[DS3231 часовий модуль для Arduino](https://lastminuteengineers.com/ds3231-rtc-arduino-tutorial/)
[Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
[Arduino - Time](https://www.tutorialspoint.com/arduino/arduino_time.htm)