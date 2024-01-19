---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що й навіщо?
Друкований вивід для налагодження - це спосіб, за допомогою якого програмісти виводять інформацію про стан програми. Ми це робимо аби краще розуміти, що відбувається всередині коду під час його виконання.

## Як це робити:
Використовуйте код нижче для створення виводу налагодження в Arduino з допомогою функції `Serial.print()`.

```Arduino
void setup() {
  // start serial port at 9600 bps:
  Serial.begin(9600);
}

void loop() {
  int sensorValue = analogRead(A0);
  // print out the value of the sensor:
  Serial.print("Sensor = " );
  Serial.println(sensorValue);
  delay(1000);
}
```
Цей код буде надсилати дані сенсора на серійний порт кожні 1 секунду. 

## Поглиблений розгляд
Друкований вивід для налагодження є старим методом, який використовують разом зі значеннями змінних, що відображаються в консолі. Є альтернативи, такі як використання вбудованих налагоджувачів, які можуть надати більш детальну інформацію про стан програми. Однак, для простих завдань або пристроїв з обмеженими ресурсами, друкований вивід для налагодження - це швидкий та дієвий метод.

## Дивись ще
1. [серійний монітор Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
2. [Альтернативи до друкованого виводу налагодження](https://arduino.stackexchange.com/questions/1180/what-are-the-alternatives-to-the-arduino-ide-for-programming-an-arduino)
3. [Історія друку виводу налагодження](https://stackoverflow.com/questions/46762601/why-do-we-need-debugging-why-not-just-use-system-out-println)