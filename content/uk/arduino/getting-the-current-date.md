---
title:                "Arduino: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Існує багато ситуацій, коли потрібно отримати поточну дату в своїх Arduino проєктах. Наприклад, ви можете використовувати цю інформацію для виведення на екран дати старту вашого проєкта або для автоматичного оновлення графіку. Також це може бути потрібно для зручності управління приладами, які потребують налаштування часу.

## Як

Програмування отримання поточної дати на Arduino досить просте. Спочатку необхідно підключити бібліотеку "TimeLib.h" до вашого скетчу. Далі за допомогою функції "now()" ви можете отримати об'єкт структури "tm" з поточними даними дати та часу.

```Arduino 
#include <TimeLib.h>

void setup() {
  // ініціалізація з'єднання зі знешнього модуля часу, наприклад DS1307
  setSyncProvider(RTC.get); 
  // очікування синхронізації зі знешнім модулем
  while (timeStatus() != timeSet);
}

void loop() {
  // отримуємо об'єкт структури "tm" з поточною датою та часом
  tmElements_t now = now(); 
  
  // виведення дати в форматі "DD/MM/YYYY"
  Serial.print(now.Day);
  Serial.print("/");
  Serial.print(now.Month);
  Serial.print("/");
  Serial.println(now.Year);
  
  // виведення часу в форматі "HH:MM:SS"
  Serial.print(now.Hour);
  Serial.print(":");
  Serial.print(now.Minute);
  Serial.print(":");
  Serial.println(now.Second);

  // затримка для оновлення даних
  delay(1000); 
}
```

В результаті ви побачите на серійному моніторі поточну дату та час.

```
25/03/2020
15:30:45
```

## Глибші дослідження

Якщо ви бажаєте отримувати більше даних про дату та час, ви можете використати інші функції із бібліотеки "TimeLib.h". Наприклад, функцію "month()" для отримання номера місяця або "hourFormat12()" для отримання годин у 12-годинному форматі. Також ви можете змінювати налаштування часу за допомогою функцій "setTime()" та "setTimezone()".

## Дивись також

- [Офіційна документація бібліотеки TimeLib.h](https://github.com/PaulStoffregen/Time)
- [Відеоурок "Робота з датою та часом на Arduino"](https://www.youtube.com/watch?v=GtlxUo81IHE)
- [Стаття "Основи роботи зі знешнім модулем часу на Arduino"](https://lobach.info/arduino-working-with-real-time-clock/)