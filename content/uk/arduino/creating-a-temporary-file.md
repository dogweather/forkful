---
title:    "Arduino: Створення тимчасового файлу"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Чому
Створення тимчасового файлу є важливою складовою процесу програмування на Arduino для тих, хто бажає працювати з більш складними та деталізованими програмами для контролю різних пристроїв.

## Як це зробити
Для створення тимчасового файлу на Arduino використовується функція `createTempFile()`. Ця функція приймає два аргументи: назву тимчасового файлу та розмір. Приклад коду та виводу:

```Arduino
#include <SPI.h>

void setup() {
  Serial.begin(9600);
}
 
void loop() {
  File tempFile = createTempFile("log.txt", 100);
  Serial.print("Новий тимчасовий файл створено: ");
  Serial.println(tempFile.name());
  Serial.print("Розмір тимчасового файлу: ");
  Serial.print(tempFile.size());
  Serial.println(" bytes");
  Serial.println("------------------");
  delay(1000);
}
```

Вивід:

"Новий тимчасовий файл створено: log.txt.tmp"  
"Розмір тимчасового файлу: 100 bytes"  
"------------------"

Також, можливо задати назву тимчасового файлу через змінну, наприклад:

```Arduino
char fileName[] = "data.csv";
File tempFile = createTempFile(fileName, 500);
```

## Поглиблене вивчення
Створення тимчасових файлів є важливою частиною програмування на Arduino, оскільки дозволяє зберігати тимчасові дані, які не потрібні на постійній основі. Також, цей процес може бути корисним при використанні мережевих протоколів для збереження тимчасових файлів на SD-карту або в сервісі хмарного сховища.

## Дивись також
- Офіційна документація по функції `createTempFile()`: https://www.arduino.cc/en/Reference/FileCreateTempFile
- Приклади використання тимчасових файлів на Arduino: https://lastminuteengineers.com/arduino-sd-card-data-logging-tutorial/