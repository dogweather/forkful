---
title:                "Читання текстового файлу."
html_title:           "C++: Читання текстового файлу."
simple_title:         "Читання текстового файлу."
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що & чому?

Читання текстового файлу - це процес отримання текстової інформації з файлу на комп'ютері. Це одна з основних дій, які виконують програмісти під час роботи з даними.

## Як:

Щоб прочитати текстовий файл у програмі, вам потрібно скористатися функцією ```ifstream```, яка дозволяє відкрити файл для читання. Потім використовуйте цю функцію для отримання даних з файлу та збереження їх у змінній. Наприклад:

```
#include <iostream>
#include <fstream>

using namespace std;

int main(){ 
  ifstream inputFile("file.txt");

  if(inputFile.is_open()){ 
    string data; 
    inputFile >> data;
    cout << "Data from file: " << data << endl;
    inputFile.close();
  }
  else {
    cout << "Unable to open file!" << endl;
  }

  return 0;
}
```

Вихідний файл:

```bash
Data from file: Hello world!
```

## Поглиблення:

Читання текстового файлу є необхідною частинкою роботи з даними в програмуванні. Розглянемо приклад, коли вам потрібно зчитати великий файл з даними. Якщо ви будете використовувати функцію ```ifstream```, збереження великої кількості даних в пам'яті комп'ютера може призвести до зайвого використання ресурсів та замовкливого програмного збою. У таких ситуаціях краще використовувати потоковий метод читання даних.

## Дивись також:

- [Базові функції у C++](https://www.programiz.com/cpp-programming/input-output)
- [Читання та запис файлів у C++](https://www.geeksforgeeks.org/reading-and-writing-files-in-c/)
- [Створення та редагування текстових файлів з командного рядка](https://linuxize.com/post/how-to-create-edit-and-manage-text-file