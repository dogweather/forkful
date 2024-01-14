---
title:    "Arduino: Писання до стандартного виводу помилок"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Чому
Запис до стандартного виводу помилки є важливим елементом програмування на Arduino. Це дозволяє отримувати спеціальні повідомлення про помилки, які допомагають відлагоджувати програми та забезпечувати правильну роботу ваших проектів.

## Як
Щоб записувати до стандартного виводу помилки, потрібно використовувати команду `Serial.println()` у своїх програмах. Наприклад:

```Arduino
Serial.println("Це повідомлення буде записано до стандартного виводу помилки");
```

В результаті ви побачите таке повідомлення у консолі виводу при відлагодженні ваших проектів або у вікні монітора порта, якщо підключенів використовувати Serial monitor.

## Глибоке погруження
Запис до стандартного виводу помилки допомагає відлагоджувати програми і забезпечувати їх правильну роботу. Ви також можете змінювати рівні виводу помилки для зручності. Наприклад, ви можете використовувати `Serial.print()` для виведення проміжних значень до стандартного виводу помилки та визначати рівень запуску у консолі виводу.

Просунуті користувачі можуть також використовувати бібліотеку `SoftwareSerial` для підключення додаткових пристроїв до інших портів, таких як RX та TX.

## Дивіться також
- [Diving Deep: Debugging Arduino Code](https://learn.sparkfun.com/tutorials/diving-deep-debugging-arduino-code) - стаття про різні способи відлагодження програм на Arduino.
- [Documentation on `Serial.println()`](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/) - документація про команду `Serial.println()` на офіційному сайті Arduino.
- [SoftwareSerial Library](https://www.arduino.cc/en/Reference/SoftwareSerial) - офіційна документація про бібліотеку `SoftwareSerial`.