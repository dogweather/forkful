---
title:    "Arduino: Отримання поточної дати"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

У цій статті розповімо про те, як отримати поточну дату на вашому ардуіно. Це дозволить вам орієнтуватися у часі та використовувати поточну дату для різних цілей у вашому проекті.

## Як

```Arduino
#include <DS3231.h> //Підключаємо бібліотеку для роботи з часом
DS3231 rtc; //Задаємо змінну для роботи з RTC модулем

void setup() {
  rtc.begin(); //Ініціалізуємо модуль
  //Ця частина необов'язкова, але корисна - регулюємо час модуля на комп'ютері
  rtc.setDateTime(__DATE__, __TIME__);
}

void loop() {
  //Отримуємо поточну дату та час
  DateTime now = rtc.getDateTime();
  //Виводимо отримані дані на монітор
  Serial.print("Поточна дата: ");
  Serial.print(now.year, DEC);
  Serial.print('/');
  Serial.print(now.month, DEC);
  Serial.print('/');
  Serial.print(now.day, DEC);
  Serial.print('\t');
  Serial.print("Поточний час: ");
  Serial.print(now.hour, DEC);
  Serial.print(':');
  Serial.print(now.minute,DEC);
  Serial.print(':');
  Serial.print(now.second, DEC);
  Serial.println();
  //Затримуємо виконання програми на 1 секунду
  delay(1000);
}
```

Після завантаження програми на ардуіно та підключення до комп'ютера, ви побачите вихідні дані на моніторі - поточну дату та час, зчитані з RTC модуля. У разі, якщо ви бачите неправильну дату або час, спробуйте відрегулювати їх на комп'ютері, як показано у `setup()` функції.

## Поглиблене вивчення

Якщо ви хочете більш глибоко вивчити роботу з датою та часом на ардуіно, варто звернути увагу на наступні моменти:

- Бібліотека `DS3231.h` використовує керування I2C, тому ваш ардуіно повинен мати відповідні інтерфейси;
- Можливо, ви захочете створити зручну функцію для виведення дати та часу безпосередньо на LCD дисплей;
- Якщо ви хочете, щоб програма друкувала логи окремо від основного функціоналу, варто вивчити роботу з `Serial.println()` функцією.

## Дивись також

- [Бібліотека DS3231.h на GitHub](https://github.com/NorthernWidget/DS3231)
- [Туторіал з роботи з датою та часом на ардуіно](https://www.programmingelectronics.com/how-to-use-real-time-clock-module-arduino/?cn-reloaded=1)