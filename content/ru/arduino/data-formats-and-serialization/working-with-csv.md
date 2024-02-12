---
title:                "Работа с CSV"
aliases: - /ru/arduino/working-with-csv.md
date:                  2024-01-29T00:03:39.403467-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с CSV (значения, разделенные запятыми) на Arduino позволяет хранить и управлять данными в текстовом формате. Это дешево, просто и универсально, что делает его идеальным для регистрации данных, файлов конфигурации или для обмена данными с таблицами и базами данных.

## Как это сделать:
Вот как сохранить данные сенсора в файл CSV на SD-карте:

```Arduino
#include <SD.h>
#include <SPI.h>

File myFile;
int sensorValue = analogRead(A0);  // предполагаемое значение сенсора

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {  // SD-карта подключена к пину 4
    Serial.println("Не удалось обнаружить SD-карту или она отсутствует");
    return;
  }
  
  myFile = SD.open("data.csv", FILE_WRITE);
  
  if (myFile) {
    myFile.print("Время, ЗначениеСенсора\n");
    unsigned long time = millis();
    myFile.print(time);
    myFile.print(", ");
    myFile.print(sensorValue);
    myFile.close();
    
    Serial.println("Данные записаны на SD-карту.");
  } else {
    Serial.println("Ошибка открытия файла для записи.");
  }
}

void loop() {
  // Здесь делать нечего
}
```

Пример вывода в CSV в `data.csv`:
```
Время, ЗначениеСенсора
12345, 678
```

## Погружение в тему
Формат CSV можно отследить до ранних дней вычислительной техники. Несмотря на наличие более изысканных альтернатив, таких как JSON или XML, CSV остается предпочтительным вариантом из-за его простоты и широкой поддержки на различных платформах. Работая с Arduino, следует учитывать ограниченную память и выбирать минималистичные библиотеки CSV или создавать собственные функции для эффективного анализа и генерации данных CSV.

## Смотрите также
- Справочник по библиотеке SD для Arduino: https://www.arduino.cc/en/reference/SD
- Простой анализ CSV на C: https://github.com/robertgamble/simplecsv
- Учебник по сохранению данных с Arduino в Excel: https://www.instructables.com/Save-Arduino-sensor-data-to-a-text-file/
