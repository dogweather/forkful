---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?
Діставання підрядків - це процес видобутку структурованих фрагментів з великого рядка. Програмісти роблять це, щоб працювати з менше складними даними чи виконувати пошук у рядку.

## Як це зробити:
```Arduino
String name = "Programming with Arduino"; 
String substringName = name.substring(0, 11); 
Serial.println(substringName);
```
Скопіювавши та виконавши даний код, ви побачите на виході:
```Arduino
Programming
```
На першому рядку ми оголосили рядок "Programming with Arduino". На другому рядку ми створили підрядок від індексу 0 до 11. На третьому рядку ми виводимо цей підрядок.

## Пірнемо глибше
Екстракція підрядків - це просто та дієвий спосіб обробки рядків, що використовується з самого зародження мов програмування. В Arduino є альтернативи, особливо при роботі з символьними масивами (char arrays), але метод substring() є найпростішим для розуміння та використання. Щойно викликано метод substring(), Arduino створює новий об'єкт String, копіює витрібний фрагмент в цей об'єкт, та повертає його. 

## Дивіться також:
[Arduino String Objects](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/) - детальна документація про рядкові об'єкти в Arduino.
[String Manipulation](https://learn.adafruit.com/adafruit-arduino-lesson-22-string-manipulation/overview) - основи роботи з рядками на Arduino. Робота з підрядками тут є частиною більшої картини.