---
title:                "Приведення рядка до верхнього регістру"
html_title:           "Arduino: Приведення рядка до верхнього регістру"
simple_title:         "Приведення рядка до верхнього регістру"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Той, чому
Використання функції для перетворення тексту великими літерами є необхідною процедурою при роботі з різними даними, що вимагають однорідного форматування. Це може бути корисно при створенні, наприклад, дисплею з інформацією або запуску процесу відтворення музики.

## Як
Для того, щоб перетворити рядок великими літерами у мові Arduino, використовуйте функцію `toUpperCase()`. Нижче наведено приклад коду та вихідний результат, який відображується на моніторі:

```Arduino
String text = "arduino";
text.toUpperCase();
Serial.println(text); // виводить "ARDUINO" на моніторі
```

## Глибоке дослідження
Код `toUpperCase()` використовується для перетворення рядка великими літерами шляхом заміни кожної малий літери на відповідну велику літеру за кодом ASCII. Це може бути виконано за допомогою циклу, але використання готової функції є більш зручним та ефективним способом.

## Дивись також
- [Документація по функції toUpperCase()](https://www.arduino.cc/reference/uk/language/functions/string/uppercase/)
- [Код ASCII](https://www.w3schools.com/charsets/ref_html_ascii.asp)
- [Приклад застосування toUpperCase() для керування світлодіодами](https://create.arduino.cc/projecthub/ailiev/use-the-toupper-function-to-control-an-led-870c48)