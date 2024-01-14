---
title:    "C++: Створення тимчасового файлу."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу є корисним для збереження тимчасових даних, які потрібно використати в певному процесі програми.

## Як

```C++
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main() {
   //Створення тимчасового файлу
   ofstream tempFile;
   tempFile.open("temp.txt");

   //Запис даних у тимчасовий файл
   tempFile << "Це тимчасовий файл.";

   //Закриття файлу
   tempFile.close();

   //Відкриття файлу для читання
   ifstream inputFile;
   inputFile.open("temp.txt");

   //Виведення даних з тимчасового файлу на екран
   char tempOutput[500];
   if(inputFile.is_open()){
       while(!inputFile.eof()){
           inputFile >> tempOutput;
           cout << tempOutput << " ";
       }
   }

   //Закриття файлу
   inputFile.close();

   return 0;
}
```

Виведення:

Це тимчасовий файл.

## Глибокий аналіз

Тимчасові файли є особливим типом файлів в програмуванні. Вони автоматично зникають після завершення програми або після закриття файлу. Існує кілька розширень для тимчасових файлів, таких як .tmp, .temp, .bak, .old тощо. Також, тимчасові файли можуть бути створені у різних директоріях залежно від операційної системи та налаштувань.

## Див. також

- [Документація C++ про роботу з файлами](https://www.cplusplus.com/doc/tutorial/files/)
- [Створення тимчасового файлу в Java](https://www.geeksforgeeks.org/java-program-to-create-a-temporary-file/) 
- [Різні типи тимчасових файлів у Windows](https://www.lifewire.com/temporary-files-in-windows-2626029)