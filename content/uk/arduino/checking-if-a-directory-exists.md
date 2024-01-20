---
title:                "Перевірка наявності директорії"
html_title:           "Arduino: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що й навіщо?

Перевірка наявності директорії в Arduino - це процес, коли програма визначає, чи існує певна директорія у файловій системі чи ні. Розробники роблять це, щоб уникнути помилок, які можуть виникнути під час спроби доступу до неіснуючої директорії.

## Як це робити:

Фрагменти коду Arduino та приклади виводу знаходяться у блоках коду Arduino:

```Arduino
#include <SPI.h>
#include <SD.h>

File root;

void setup()
{
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("initialization failed!");
    return;
  }
  Serial.println("initialization done.");
  
  root = SD.open("/");

  if (SD.exists("example.txt")){
    Serial.println("example.txt exists.");
  }
  else {
    Serial.println("example.txt doesn't exist.");
  }
}
```
Вивід:

```
initialization done.
example.txt exists.
```
## Пірнання глибше

Перевірка наявності директорії не є новим поняттям в програмуванні. Ця концепція має свої коріння з часів, коли файлові системи стали неодмінною частиною операційних систем, що вимагало більш конкретного керування файлами і дирeктриями.

Існує декілька альтернативних методів перевірки наявності директорії в Arduino, включаючи використання бібліотеки `FileIO` (для плат Arduino Yún), але метод `SD.exists()` є найпростішим використовувати.

Ви можете бачити, що вище наведений код працює в основному з об'єктом `File root`, який представляє корінь файлової системи SD картки. Потім використовується метод `SD.exists()`, щоб перевірити, існує файл чи ні.

## Дивіться також

Ардуіно: Аплікація SD.exists() - https://www.arduino.cc/en/Reference/SD_exists

Arduino SD бібліотека - https://www.arduino.cc/en/Reference/SD

Arduino файла система документація - https://www.arduino.cc/en/Guide/Environment#toc13