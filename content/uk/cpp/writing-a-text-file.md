---
title:                "Написання текстового файлу"
html_title:           "C++: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

Що: Чому б комусь займатися написанням текстового файлу в мові C++?

Так: Для написання текстового файлу в мові C++, вам спочатку потрібно вивчити основні концепції щодо читання і запису файлів. Далі, ви можете використовувати функції `ofstream` та `ifstream` для запису та читання файлів відповідно.

```C++
#include <iostream>
#include <fstream>

int main(){
  // Створюємо об'єкт, який буде записувати у файл
  std::ofstream myfile;
  // Відкриваємо файл для запису, замінюючи попередній вміст
  myfile.open("example.txt");
  // Записуємо текст у файл
  myfile << "Це приклад тексту, який буде записаний у файл." << std::endl;
  // Закриваємо файл
  myfile.close();

  // Створюємо об'єкт, який буде читати з файлу
  std::ifstream myfile2;
  // Відкриваємо файл для читання
  myfile2.open("example.txt");
  // Створюємо змінну для збереження змісту файлу
  std::string content;
  // Читаємо з файлу та зберігаємо у змінну
  myfile2 >> content;
  // Виводимо зміст на екран
  std::cout << "Зміст файлу: " << content << std::endl;
  // Закриваємо файл
  myfile2.close();
}
```

### Поглиблений аналіз

Крім функцій `ofstream` та `ifstream`, мова C++ також має функції `fstream` для взаємодії з файлами, які можуть одночасно записувати та читати. Крім того, для запису використовується оператор `<<`, а для читання - оператор `>>`. Використовуйте функцію `open()` для відкриття файлу та `close()` для закриття. Також важливо переконатися, що файл успішно відкрився, перевіривши повернене значення функції `open()`.

### Дивіться також

- [Документація по функціям `ofstream`, `ifstream` та `fstream`](https://www.cplusplus.com/reference/fstream/)
- [Вказівники у мові C++](https://www.geeksforgeeks.org/pointers-in-c-and-cpp/)
- [Робота з текстовими файлами в мові C++](https://www.studytonight.com/cpp/writing-and-reading-text-files-in-cpp.php)