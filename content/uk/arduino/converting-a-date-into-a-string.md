---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Конвертація дати у рядок означає перетворення об'єкта дати у текстові дані. Програмісти роблять це, щоб завантажити, відобразити або відправити дату в текстовому форматі.

## Як зробити: 

Для конвертації дати в рядок використовується функція `sprintf()`. Нижче наведено приклад коду Arduino.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(8, 29, 0, 2, 1, 2011); 
}

void loop() {
  char buffer[50];
  sprintf(buffer, "%02d/%02d/%d %02d:%02d:%02d", day(), month(), year(), hour(), minute(), second());
  Serial.println(buffer);
  delay(1000);
}
```

В результаті виходу отримуємо:

```Arduino
02/01/2011 08:29:00
```
## Занурення в глибину:

1. Історичний контекст: Функція `sprintf()` вперше з'явилася в мові програмування C та потім була інтегрована в Arduino.

2. Альтернативи: Крім `sprintf()`, можна використовувати `String()` для створення рядків в Arduino.

3. Деталі реалізації: Функція `sprintf()` використовує рядок формату, щоб визначити як конвертувати дату в рядок. Символи типу '%d', '%02d' вказують формат виводу даних.

## Бачте також: 

- [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
- [Arduino sprintf](https://www.arduino.cc/reference/en/language/functions/characters/printf/)
- [Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)