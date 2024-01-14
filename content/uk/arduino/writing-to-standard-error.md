---
title:                "Arduino: Написання до стандартного виводу помилок"
simple_title:         "Написання до стандартного виводу помилок"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Чому
В програмуванні на Arduino часто можна зіткнутися з помилками, але не завжди легко зрозуміти, що саме спричинило цю помилку. Використання стандартного помічника для виведення помилок може допомогти в цій ситуації.

## Як це зробити
```Arduino
Serial.println("Це повідомлення буде виведено у вікно для помилок (standard error)");
```

Вище наведений код дозволяє вивести повідомлення у вікно для помилок, що може допомогти відлагоджувати програми та зрозуміти причину помилок у коді.

## Глибока погруження
Використання ```Serial.println()``` дозволяє також виводити значення змінних для подальшого аналізу. Наприклад:

```Arduino
int sensorValue = analogRead(A0); // зчитування значення з аналогового піна A0
Serial.println("Значення з аналогового піна A0: ");
Serial.println(sensorValue); // виведення значення у вікно для помилок
```

Цей код дозволяє вивести значення, яке було зчитано з аналогового піна, що може бути корисним для перевірки правильності отриманих даних.

## Дивись також
- [Офіційна документація Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Корисні поради для дебаггінгу на Arduino](https://www.megunolink.com/articles/arduino-debugging/#serialoutput)
- [Відеоуроки з програмування на Arduino](https://www.youtube.com/playlist?list=PLXdcO6Oe-DlXzflc27N56VgaxbclIdJEc)