---
title:                "Отримання поточної дати"
html_title:           "Arduino: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Що & Чому?
Отримання поточної дати - це процес отримання поточної дати і часу на пристрої, такому як Arduino. Програмісти зазвичай роблять це для того, щоб встановити точний час на пристрої або для подальших обчислень, пов'язаних зі зміною часу.

# Як:
Програмування дати в Arduino дуже просте. Для початку нам потрібно підключити бібліотеку для роботи з датою і часом. Нижче наведений код, який допоможе вам отримати поточну дату і час на Arduino.

```Arduino
#include <TimeLib.h>
#include <DS3231.h>

DS3231 rtc;

void setup() {
  Serial.begin(9600);
  rtc.begin();
}

void loop() {
  // отримуємо поточну дату і час
  int year = year();
  int month = month();
  int day = day();
  int hour = hour();
  int minute = minute();
  int second = second();
  
  // виводимо результати в моніторі
  Serial.print("Поточна дата і час: ");
  Serial.print(month); 
  Serial.print("/");
  Serial.print(day);
  Serial.print("/");
  Serial.print(year);
  Serial.print(" ");
  Serial.print(hour);
  Serial.print(":");
  Serial.print(minute);
  Serial.print(":");
  Serial.println(second);

  // чекаємо 1 секунду перед повторною перевіркою
  // важливо, щоб не перевантажувати пристрій
  delay(1000); 
}
```

Ви можете змінити код із виведенням результатів, як вам потрібно, або використовувати отримані значення для подальших обчислень.

# Глибокий погляд:
Отримання поточної дати на Arduino є важливою задачею для багатьох проектів, пов'язаних зі зміною часу і точністю. Існують різні способи отримання дати, включаючи використання вбудованого годинника реального часу (RTC) або з'єднання з Інтернетом для отримання дати і часу з сервера NTP.

Крім того, також існують деякі практичні випадки, коли потрібно настроїти годинник RTC на пристрої, щоб встановити точний час. У таких випадках можна використовувати кнопку для настроювання інтерактивно або використовувати внутрішній таймер для автоматичного настроювання.

# Дивись також:
- [Бібліотека TimeLib для Arduino](https://www.arduino.cc/reference/en/libraries/timelib/)
- [Бібліотека DS3231 для роботи з годинником реального часу](https://github.com/jchristensen/DS3232RTC)
- [Керуючий режим годинника реального часу](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Digital/Clock)