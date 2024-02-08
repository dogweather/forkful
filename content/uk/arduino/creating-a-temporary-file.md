---
title:                "Створення тимчасового файлу"
aliases:
- uk/arduino/creating-a-temporary-file.md
date:                  2024-01-20T17:40:34.308064-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Тимчасові файли - це файли, створені для тимчасового зберігання інформації, як правило, під час виконання програми. Програмісти використовують їх для збереження проміжних даних, уникнення зайвих записів у головний файл та зменшення використання оперативної пам’яті.

## Як робити:

Arduino не працює із тимчасовими файлами так, як це робить традиційна операційна система. Замість цього, ми можемо емулювати тимчасове зберігання, використовуючи EEPROM або SD карту для запису тимчасових даних.

```Arduino
#include <SD.h>

File myTempFile;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin()) {
    Serial.println("SD card initialization failed!");
    return;
  }

  myTempFile = SD.open("temp.txt", FILE_WRITE);

  if (myTempFile) {
    myTempFile.println("This is a temporary line of text.");
    
    // Always make sure to close the file, so that the data gets written to the SD card.
    myTempFile.close();
  }
}

void loop() {
 // Code would go here to handle the temporary file as needed.
}
```

## Глибше занурення:

Традиційно, тимчасові файли використовують у ОС з файловою системою. Arduino не володіє такою системою за замовчуванням. Але за допомогою SD-картки та спеціального щита SD можна створювати, читати та писати файли. Історично SD-картка надає простий спосіб для розширення можливостей зберігання даних для мікроконтролерів. Альтернативою тимчасовому зберіганню на SD-картці може бути EEPROM — внутрішня пам'ять Arduino, придатна для зберігання невеликих обсягів даних.

## Дивіться також:

- Офіційна документація по роботі з SD-картами Arduino: https://www.arduino.cc/en/Reference/SD
- Робота з EEPROM в Arduino: https://www.arduino.cc/en/Reference/EEPROM
- Додаткові відомості про файлові системи: https://en.wikipedia.org/wiki/File_system
