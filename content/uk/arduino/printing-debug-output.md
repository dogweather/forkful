---
title:                "Arduino: Виведення відладочного виводу"
simple_title:         "Виведення відладочного виводу"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

Навіщо нам використовувати вивід відладки (debug output) в програмуванні на Arduino? Необхідно зрозуміти, що вивід відладки дозволяє нам докладніше вивчати роботу нашої програми та виявляти помилки.

## Як це зробити

Для того, щоб отримати вивід відладки на Arduino, необхідно скористатися функцією Serial.print(). Нижче приведений приклад коду, який демонструє вивід числа на монітор послідовного з'єднання:

```Arduino
int num = 5;
Serial.print("My number is: ");
Serial.print(num);
```

В результаті ви побачите такий вивід на моніторі:

```
My number is: 5
```

Також ви можете використовувати функцію Serial.println() для того, щоб вивести число на новому рядку:

```Arduino
int num = 5;
Serial.println("My number is: ");
Serial.println(num);
```

Результат буде виглядати так:

```
My number is:
5
```

## Глибокий погляд

Якщо хто-небудь з вас попередньо програмував на С++ або Java, ви, можливо, знаєте про функції printf та println, які можуть бути корисними при виведенні відладки. Функції Serial.print та Serial.println насправді є аналогами відповідних функцій мов програмування, які ви полюбляєте. Зокрема, що потрібно помічати, це відсутність визначення типів змінних у функції Serial.print. Також варто зазначити, що функція Serial.println автоматично виводить новий рядок після виведення значення.

## Дивись також

- [Офіційна документація з функцій виводу відладки на Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Відладка: використання виводу даних в проекті Arduino](https://techtutorialsx.com/2019/01/14/arduino-printing-data-output-to-the-serial-monitor/)
- [Відлагодження програм на Arduino з використанням виводу відладки](https://create.arduino.cc/projecthub/atsurya95/a-debugging-tool-for-arduino-fb17a7)

## Дивись також

- [Офіційна документація з функцій виводу відладки на Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Відладка: використання виводу даних в проекті Arduino](https://techtutorialsx.com/2019/01/14/arduino-printing-data-output-to-the-serial-monitor/)
- [Відлагодження програм на Arduino з використанням виводу відладки](https://create.arduino.cc/projecthub/atsurya95/a-debugging-tool-for-arduino-fb17a7)