---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це і навіщо?
Інтерполяція рядків полягає в вставці змінних або виразів всередину рядка. Програмісти використовують це для динамічного форматування інформації, що робить код більш зрозумілим і охайним.

## Як це робиться:
Простий приклад використання інтерполяції рядків в Arduino коді:

```Arduino
String name = "Володимир";
String greeting = String("Привіт, ") + name + String("!");
Serial.println(greeting); // Виводимо: Привіт, Володимир!
```

## Занурюємося глибше
Історичний контекст: Інтерполяція рядків використовувалася в програмуванні ще з часів мов Perl і Python, де ця ідея була широко прийнята.

Альтернативи: Ви також можете використовувати `snprintf` або `sprintf` для форматування рядків, але це може бути менш інтуїтивно.

Інформація про реалізацію: Зауважте, що в Arduino інтерполяція рядків реалізована через перевантаження оператора '+'. Це означає, що ви можете використовувати '+' для додавання рядків, але це використовує більше ресурсів, ніж `snprintf`.

## Дивіться також:
- [Arduino - String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino - Serial.print](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)