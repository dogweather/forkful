---
title:                "Створення текстового файлу"
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Текстовий файл - це файл зі зчитуваним текстом. Програмісти пишуть до файлу дані для логування, збереження налаштувань, та обміну даними.

## How to:
```C
#include <stdio.h>

int main() {
    FILE *file = fopen("example.txt", "w");
    if (file == NULL) {
        return 1; // Не вдалося відкрити файл
    }
    
    fprintf(file, "Привіт, світ!\n"); // Запис у файл
    fclose(file); // Закриття файлу
    
    return 0;
}
```
Output (`example.txt`):
```
Привіт, світ!
```

## Deep Dive
Запис у файл у C здавна є стандартним способом збереження даних. Альтернативою може бути `fwrite` для бінарних файлів. Деталі реалізації включають буферизацію та обробку помилок.

## See Also
- Official C Documentation: [Writing Files in C](https://en.cppreference.com/w/c/io)
- C Programming Tutorial on File I/O: [Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)