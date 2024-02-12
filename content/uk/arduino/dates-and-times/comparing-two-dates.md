---
title:                "Порівняння двох дат"
aliases: - /uk/arduino/comparing-two-dates.md
date:                  2024-01-20T17:32:17.663009-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Навіщо?)
Сравнення двох дат ― це визначення, яка з дат раніша, пізніша, чи вони однакові. Програмісти роблять це для відслідковування подій, змін у даних часу, таймінгу задач.

## How to: (Як це зробити:)
Уявимо, вам треба порівняти сьогоднішню дату з дедлайном. В Arduino можна використовувати бібліотеку TimeLib для роботи з часом.

```Arduino
#include <TimeLib.h> // Підключення бібліотеки Time

void setup() {
  Serial.begin(9600);
  // Тут ініціалізация часу
  setTime(12, 34, 56, 1, 2, 2023); // 12:34:56 1 лютого 2023
}

void loop() {
  time_t now = now(); // Отримання поточного часу
  time_t deadline = makeTime(12, 0, 0, 15, 2, 2023); // 12:00:00 15 лютого 2023

  if (now > deadline) {
    Serial.println("Дедлайн пропущено!");
  } else if (now < deadline) {
    Serial.println("Є ще час до дедлайну.");
  } else {
    Serial.println("Сьогодні дедлайн!");
  }
  delay(1000); // Чекаемо 1 секунду
}
```

## Deep Dive (Поглиблене занурення)
Порівняння дат у мікроконтролерів, як Arduino, багато в чому залежить від часового хардвару і настроювання. TimeLib є однією з доступних бібліотек, але є й інші, як RTClib для реального часу.

В історичному контексті, стеження за часом було складно без вбудованих годинників. Arduino Uno і такі подібні плати не мають RTC (Real Time Clock), тому треба або записувати час при завантаженні скетчу, або використовувати зовнішні модулі RTC.

Щодо реалізації, `time_t` це базовий тип даних у TimeLib, що представляє час у секундах з певної епохи (зазвичай 1 січня 1970). Функція `makeTime` конвертує зручні формати (година, дата, місяць, рік) у `time_t`. Сравнення потім робиться за допомогою стандартних операторів (`<`, `>`, `==`).

## See Also (Дивіться також):
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Arduino RTC Library](https://www.arduino.cc/en/Reference/RTC)
- [UNIX Time and Date](https://en.wikipedia.org/wiki/Unix_time)
