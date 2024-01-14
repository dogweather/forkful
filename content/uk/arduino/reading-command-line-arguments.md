---
title:                "Arduino: Читання аргументів командного рядка"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Навіщо комусь читати аргументи командного рядка при програмуванні на Arduino? Це важливий навик, який дозволяє збільшити можливості і ефективність ваших проектів. Детальніше про це – у нашій статті.

## Як

При програмуванні на Arduino, ви можете скористатися аргументами командного рядка для передачі параметрів вашій програмі. Для цього використовується функція `getArgs()`, яка повертає значення, передані в командному рядку. Наприклад:

```Arduino
void setup() {
  // ініціалізація вашої програми
  Serial.begin(9600);
  Serial.println("Введіть свій вік:");
  int age = getArgs().toInt();
  Serial.print("Вам ");
  Serial.print(age);
  Serial.println(" років.");
}
```

Вводити аргументи командного рядка можна через монітор послідовного порту. Наприклад, якщо ви введете "25" у моніторі послідовного порту, програма виведе "Вам 25 років."

## Глибоке занурення

Окрім передачі числових значень, ви також можете передавати рядкові значення через аргументи командного рядка. Для цього використовується функція `getArgsString()`. Розглянемо приклад:

```Arduino
void setup() {
  // ініціалізація вашої програми
  Serial.begin(9600);
  Serial.println("Введіть своє ім'я:");
  String name = getArgsString();
  Serial.print("Привіт, ");
  Serial.print(name);
  Serial.println("!");
}
```

Якщо ви введете "Марія" у моніторі послідовного порту, програма виведе "Привіт, Марія!"

## Дивіться також

- [Документація Arduino про аргументи командного рядка](https://www.arduino.cc/reference/en/language/functions/communication/getargs/)
- [Стаття про корисні функції для програмування на Arduino](https://ua.arduino.cc/blog/5-useful-arduino-functions/)
- [Книга "Програмування на мові Arduino"](https://www.ozon.ru/context/detail/id/34520293/)