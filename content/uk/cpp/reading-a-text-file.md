---
title:    "C++: Читання текстового файлу"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Почему

Програмування стає все більш популярним одним із основних навичок у сучасному світі. І у цьому текстовому файлі ми розглянемо одну з фундаментальних операцій - читання текстового файлу. Це важлива і корисна навичка для будь-якого програміста, який працює з даними інших людей або програм.

## Як

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    // відкриваємо файл
    ifstream file("text.txt");

    // перевіряємо чи файл існує і відкрився успішно
    if (!file) {
        cerr << "Помилка! Файл не може бути відкритим!";
        exit(1);
    }

    // створюємо змінну для зберігання даних з файлу
    string data;

    // зчитуємо дані з файлу доки не досягнемо кінця файлу
    while (!file.eof()) {
        // зчитуємо рядок з файлу і записуємо його в змінну data
        getline(file, data);
        // виводимо зчитаний рядок
        cout << data << endl;
    }

    // закриваємо файл
    file.close();

    return 0;
}
```

### Вхідний файл (text.txt):
```
Привіт Україно!
Сьогодні гарний день, чи не так?
Дякую, що читаєте цей файл.
```

### Результат:
```
Привіт Україно!
Сьогодні гарний день, чи не так?
Дякую, що читаєте цей файл.
```

## Глибоке дослідження

У C++ є два основні способи читання текстового файлу - за допомогою класу `ifstream` і функції `getline()`. Клас `ifstream` дозволяє нам відкрити і читати текстовий файл, а функція `getline()` зчитує рядок з файлу і записує його в змінну.

Також важливо пам'ятати, що після використання методу `getline()`, курсор позиціонується на початку нового рядка. Щоб цього уникнути, можна використовувати метод `ignore()`, який пропускає вказану кількість символів після зчитування рядка.

## Дивись також

- [Читання та запис до текстового файлу в C++](https://www.geeksforgeeks.org/reading-writing-text-file-c/)
- [Операції з файлами у C++](https://docs.microsoft.com/uk-ua/cpp/standard-library/file-operations-in-cpp?view=vs-2019)