---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:39.403467-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u0441\u043E\u0445\u0440\u0430\
  \u043D\u0438\u0442\u044C \u0434\u0430\u043D\u043D\u044B\u0435 \u0441\u0435\u043D\
  \u0441\u043E\u0440\u0430 \u0432 \u0444\u0430\u0439\u043B CSV \u043D\u0430 SD-\u043A\
  \u0430\u0440\u0442\u0435."
lastmod: '2024-03-13T22:44:45.573488-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u0441\u043E\u0445\u0440\u0430\u043D\
  \u0438\u0442\u044C \u0434\u0430\u043D\u043D\u044B\u0435 \u0441\u0435\u043D\u0441\
  \u043E\u0440\u0430 \u0432 \u0444\u0430\u0439\u043B CSV \u043D\u0430 SD-\u043A\u0430\
  \u0440\u0442\u0435."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

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
