---
title:                "Arduino: Перевірка наявності каталогу"
simple_title:         "Перевірка наявності каталогу"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливим кроком у програмуванні Arduino, оскільки дозволяє визначити наявність необхідних файлів та виконувати певні дії в залежності від результату.

## Як це зробити

Існує кілька способів перевірки існування директорії в Arduino. Один з таких варіантів використовує функцію `SD.exists()`, яка повертає `true` або `false` в залежності від наявності файлу у заданій директорії. Ось приклад коду та його виведення:

```Arduino
#include <SD.h>

void setup(){
  // Підключення SD-карти та ініціалізація
  if(!SD.begin()){
    while(1);
  }
}

void loop(){
  // Перевірка існування директорії '/logs'
  if(SD.exists("/logs")){
    Serial.println("Директорія існує!");
  } else {
    Serial.println("Директорія не знайдена!");
  }
  delay(1000);
}
```

Виведення:
```
Директорія існує!
```

## Глибока аналітика

Функція `SD.exists()` використовує інструкцію `fileExists()` з бібліотеки `SDlib`, яка перевіряє наявність файлу відповідним чином на SD-карті. Цей метод є досить швидким та ефективним, оскільки не потребує зчитування самого файлу, а лише інформації про його наявність у директорії.

## Дивись також

- [Офіційна документація Arduino про функцію `SD.exists()`](https://www.arduino.cc/en/Reference/SDExists)
- [Документація бібліотеки `SDlib`](https://www.arduino.cc/en/Reference/SDlib)