---
title:                "Запись в стандартный поток ошибок"
aliases: - /ru/arduino/writing-to-standard-error.md
date:                  2024-01-29T00:06:29.591462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Запись в стандартный поток ошибок (stderr) позволяет отчетам об ошибках и диагностике отделяться от стандартного вывода (stdout). Это критически важно для отладки и ведения журналов, помогая разработчикам изолировать проблемы, не смешивая сообщения об ошибках с обычным выводом программы.

## Как это сделать:
Arduino изначально не поддерживает stderr, но мы можем имитировать его, записывая в Serial. Представим программу мигания светодиодом с проверкой на ошибки:

```Arduino
void setup() {
  Serial.begin(9600);
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  if(!digitalWriteCheck(LED_BUILTIN, HIGH)) {
    Serial.println("Ошибка: Невозможно установить LED в высокое состояние"); // Это наш "stderr"
  }
  delay(1000); // Подождать секунду
  if(!digitalWriteCheck(LED_BUILTIN, LOW)) {
    Serial.println("Ошибка: Невозможно установить LED в низкое состояние"); // Это наш "stderr"
  }
  delay(1000); // Подождать секунду
}

bool digitalWriteCheck(int pin, int value) {
  // Представим, что эта функция проверяет, была ли успешной функция digitalWrite
  digitalWrite(pin, value);
  // Если успех - возвращаем true, давайте для примера всегда возвращать false
  return false;
}
```

Пример вывода:
```
Ошибка: Невозможно установить LED в высокое состояние
Ошибка: Невозможно установить LED в низкое состояние
```

## Подробнее
Исторически, stderr является стандартным потоком во многих операционных системах, введенным Unix. В Arduino, которая не имеет операционной системы, мы вручную выводим ошибки, используя Serial.print или похожие команды. Если вы ведете журнал на компьютере, логи могут быть перенаправлены из Serial в файл, эффективно отделяя их от stdout. Продвинутые пользователи могут использовать SoftwareSerial для эмуляции stderr на разных аппаратных последовательных портах.

## Смотрите также
- Официальную документацию Arduino по Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Стандартные потоки Unix: https://en.wikipedia.org/wiki/Standard_streams
- Библиотеку SoftwareSerial: https://www.arduino.cc/en/Reference/SoftwareSerial
