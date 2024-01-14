---
title:                "Arduino: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Обчислення дати в майбутньому або в минулому може бути корисним для створення програм, які зберігають інформацію про події, які повинні трапитися у певні дні.

## Як

```Arduino
#include <DateTime.h>
void setup() {
  Serial.begin(9600);
  DateTime now = DateTime(year, month, day, hour, minute, second);
  int futureDays = 30;
  DateTime futureDate = now + TimeSpan(1, 0, 0, futureDays);
  Serial.print("Дата, що наступить через 30 днів: ");
  Serial.print(futureDate.year());
  Serial.print("-");
  Serial.print(futureDate.month());
  Serial.print("-");
  Serial.print(futureDate.day());
}
```

В цьому прикладі ми використовуємо бібліотеку DateTime для створення об'єкта, який представляє поточну дату і час. Власники Arduino Uno можуть використовувати RTC модуль для отримання поточної дати та часу. Потім ми використовуємо TimeSpan, щоб додати 30 днів до поточної дати і отримати дату в майбутньому. В результаті ми отримаємо дату у форматі рік-місяць-день, який можемо вивести за допомогою Serial порту. 

## Глибше вдіваємось

Інколи нам може знадобитись обчислити дату в минулому замість майбутнього. Для цього ми можемо використати той самий підхід, але замість додавання днів ми відніматимемо їх. Також ця техніка може бути корисною для розрахунку дати наступного року або місяця. Наприклад, можна використати це для створення календаря подій або для розрахунку експірації певного терміну. 

## Дивись також

- [DateTime бібліотека](https://github.com/PaulStoffregen/DateTime)
- [Користувальницька бібліотека DateTime](https://github.com/bpetrovi/DateTime)
- [Часовий модуль реального часу для Arduino](https://www.arduino.cc/en/Tutorial/RTClib)