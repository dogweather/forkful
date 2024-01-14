---
title:    "Arduino: Порівняння двох дат"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Чому

Програмування на Arduino може бути дуже захоплюючим хобі, але ви могли б замислюватись, як порівнювати дві дати. Знаючи, що програмування дати є важливою умовою для роботи зі справжнім світом, ви будете в змозі створювати більш складні та корисні проекти з вашими Arduino.

##Як

Для порівняння двох дат на Arduino використовується спеціальна функція `compare()`. Давайте подивимось на приклад коду для зрозуміння, як це працює.

```Arduino

//declaring two date variables
int date1 = 20210905;
int date2 = 20210830;

//comparing the two dates using compare()
int result = compare(date1, date2);

//printing the result
Serial.println(result);

```

Цей код спочатку створює дві змінні дат - `date1` та `date2` - з обома змінними, які представляють рік, місяць та день в числовому форматі. Потім використовується функція `compare()` для порівняння цих дат. Як результат, змінна `result` буде мати значення -1, 0 або 1, залежно від того, чи `date1` менше, рівне або більше ніж `date2`. Наприклад, у нашому коді результат буде 1, оскільки `date1` є більшою датою.

##Глибокий занурення

У цьому прикладі ми використовували числові значення для дат, але ви можете також використовувати стрічки чи масиви для представлення дат. Також вам можуть знадобитися додаткові умови для порівняння дат, наприклад, перевірка на той самий рік чи місяць. Досліджуйте цю функцію більше, щоб створити більш складні та точні порівняння дат.

##Дивіться також

- [Документація Arduino про функцію compare()](https://www.arduino.cc/reference/en/language/functions/math/compare/)
- [Стаття про роботу з часом та датою на Arduino](https://create.arduino.cc/projecthub/arundalepaun/time-and-date-on-arduino-with-pc-laptop-connected-89df6c)
- [Відео урок про порівняння дат на Arduino](https://www.youtube.com/watch?v=GWQJTWDWYgM)