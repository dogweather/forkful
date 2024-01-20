---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що й чому?

Пошук та заміна тексту - це процес знаходження вказаного тексту (рядка) в коді та заміни його на інший. Програмісти роблять це для внесення змін, виправлення помилок або оптимізації коду.

## Як це робити:

```Arduino
// створення тексту
String txt = "Hello, World!";

// заміна тексту
txt.replace("World", "Arduino");

// друк зміненого тексту
Serial.begin(9600);
Serial.println(txt);
```
При виконанні цього коду виведе: "Hello, Arduino!"

## Поглиблено:

Пошук та заміна тексту виходить з давніх часів програмування та зазвичай використовується в редакторах коду. Існують різні альтернативи, особливо в мовах вищого рівня. Arduino використовує метод `replace()`, але в інших мовах, як Python або Java, використовуються `str.replace()` або `text.replace()` відповідно. Імплементація пошуку та заміни в основному заснована на переборі та порівнянні символів у рядку.

## Також дивіться:

1. [Докладніше про метод replace() в Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
3. [Пошук та заміна тексту в Python](https://www.w3schools.com/python/ref_string_replace.asp)
4. [Java String replace() метод](https://www.javatpoint.com/java-string-replace)