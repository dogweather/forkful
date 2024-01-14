---
title:                "Arduino: Cтворення тимчасового файлу"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Для чого

Створення тимчасового файлу може бути корисним при написанні коду на Arduino. Він дозволяє зберігати тимчасові дані, які необхідні для виконання певних операцій, і видаляти їх після виконання. Давайте розглянемо як це зробити.

## Як зробити

Створення тимчасового файлу на Arduino можна зробити за допомогою класу "File". Перед початком роботи потрібно підключити бібліотеку "SPI". Далі застосовуємо метод "open" для створення файлу з вказаним іменем і режимом запису. Слід зазначити, що ім'я файлу повинне бути унікальним для уникнення конфліктів. Потім ми можемо записати дані у файл за допомогою методу "write". Наприклад:

```Arduino
#include <SPI.h>
File myFile = SPI.open("temp.txt", FILE_WRITE);
myFile.write("Hello, world!");
myFile.close();
```

Після виконання цього коду, у кореневій папці карти пам'яті з'явиться файл "temp.txt" зі змістом "Hello, world!".

## Глибше вдивимось

Після відкриття файлу за допомогою методу "open", ми можемо вказати додаткові параметри, які визначають доступ до файлу. Наприклад, для зчитування файлу ми можемо використати режим "FILE_READ", а для додавання даних до файлу - "FILE_APPEND". Крім того, можна використовувати метод "available" для перевірки кількості доступної для читання інформації в файлі.

Також варто зазначити, що метод "open" повертає булеве значення, яке показує, чи вдалось створити файл. Це може бути корисно для перевірки правильності вказаного шляху або дозволів на запис до файлу.

## Дивись також

- [Офіційна документація Arduino про роботу з файловою системою](https://www.arduino.cc/en/Tutorial/FileSystem)
- [Стаття про роботу з файлами у Arduino на сайті Instructables](https://www.instructables.com/id/ArdFile-a-Library-for-SD-Card-and-file-access/)
- [Приклади коду для роботи з файлами на Arduino](https://circuitdigest.com/microcontroller-projects/arduino-save-data-into-text-file)

## Дивись також

- [Офіційна документація Arduino про роботу з файловою системою](https://www.arduino.cc/en/Tutorial/FileSystem)
- [Стаття про роботу з файлами у Arduino на сайті Instructables](https://www.instructables.com/id/ArdFile-a-Library-for-SD-Card-and-file-access/)
- [Приклади коду для роботи з файлами на Arduino](https://circuitdigest.com/microcontroller-projects/arduino-save-data-into-text-file)