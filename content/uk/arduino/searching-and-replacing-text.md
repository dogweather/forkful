---
title:                "Пошук та заміна тексту"
html_title:           "Arduino: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

На цій стороні як огляд, поясніть чому ми продаємо на пошук і заміну тексту. Наприклад, це може бути важливою задачею для виправлення помилок в коді Arduino, для автоматизації процесу редагування або для зміни певних параметрів в пристроях з мікроконтролерами.

## Як це зробити

Існує кілька способів знайти та замінити текст у вашому коді Arduino. Один з них - це використати функцію `replace()` з бібліотеки "String":
```
Arduino за кордоном

String рядок = "Hello Arduino!";
рядок.replace("Hello", "Привіт");
Serial.println(рядок);
// Виведе "Привіт Arduino!"
```
Ви також можете використати функцію `strstr` для пошуку рядків у рядку і заміни їх за допомогою функції `strcpy`:
```
Arduino за кордоном

char рядок[20] = "Arduino вдома";
strstr(рядок, "вдома");
strcpy(рядок, "на роботі");
Serial.println(рядок);
// Виведе "Arduino на роботі"
```

## Глибокий занурення

При використанні функції `replace()` бібліотеки "String", слід мати на увазі, що вона може працювати повільно, особливо при обробці великої кількості даних. Крім того, вона може бути досить вибагливою до пам'яті, що може призвести до проблем з буфером. Тому, якщо ви замінюєте широко використовувані рядки, можна використовувати метод `replace()` лише один раз, а потім використовувати метод `indexOf()` для пошуку і заміни інших екземплярів рядка в коді. Крім того, ви також можете використовувати функцію `strtok` для розбиття рядка на підрядки і зміни їх окремо.

## Дивіться також

Запропонуйте інші корисні статті для читання про роботу з текстом у коді Arduino. Наприклад:
- [Розбір рядків у Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strtok/)
- [Використання бібліотеки "String" у Arduino](https://www.arduino.cc/en/Reference/StringObject)
- [Функції для рядків у Arduino IDE](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)