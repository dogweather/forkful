---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Знання роботи з аргументами командного рядка допоможе вам управляти вашим проектом і контролювати його поведінку за допомогою зовнішніх параметрів.

## Як

```Arduino
void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; //очікуємо з'єднання з монітором послідовного порту
  }

  Serial.println("Hello World!"); //виводимо повідомлення "Hello World!"
  Serial.print("Number of arguments: "); //виводимо кількість аргументів командного рядка
  Serial.println(FastLED.getNumArgs()); //використовуємо функцію getNumArgs() для отримання кількості аргументів

  //виводимо всі існуючі аргументи разом з їхнім значенням
  for (int i = 0; i < FastLED.getNumArgs(); i++) {
    Serial.print("Argument "); //виводимо назву аргумента
    Serial.print(i); 
    Serial.print(": "); 
    Serial.println(FastLED.getArg(i)); //виводимо значення аргумента
  }
}

void loop() {
  //код вашої програми
}
```

Виводом у монітор послідовного порту буде:

```
Hello World!
Number of arguments: 3
Argument 0: hello
Argument 1: 1
Argument 2: 2.5

```

## Глибоке занурення

Читання аргументів командного рядка дозволяє передавати параметри у вашу програму під час її запуску. Це може бути корисно, наприклад, для налаштування поведінки програми або передачі даних. Щоб отримати більше інформації про функції, які можна використовувати для роботи з аргументами командного рядка, перегляньте [документацію Arduino](https://www.arduino.cc/reference/en/language/functions/command-line-arguments/).

## Дивіться також

- [Функції для роботи з аргументами командного рядка у Arduino](https://www.arduino.cc/reference/en/language/functions/command-line-arguments/)
- [Приклади використання аргументів командного рядка у програмах для Arduino](https://blog.arduino.cc/2013/07/10/command-line-arguments-in-arduino-sketches/)