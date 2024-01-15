---
title:                "Робота з csv"
html_title:           "C++: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні існує багато форматів для збереження та обробки даних, однак CSV є одним з найпопулярніших. Робота з CSV-файлами дуже важлива для збереження та обробки даних у текстовому форматі, що робить її незамінною для багатьох програмістів.

## Як

Для роботи з CSV-файлами вам потрібно використовувати бібліотеку "iostream" та "fstream" у своїй програмі. Після відкриття файлу для читання або запису, можна використовувати функцію "getline()" для отримання рядка з CSV-файлу. Нижче наведено приклад коду на мові C++:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    // Відкриваємо файл для читання
    ifstream file("data.csv");

    // Перевіряємо, чи файл відкрився успішно
    if (!file.is_open())
    {
        cout << "Помилка: файл не може бути відкритий!" << endl;
        return 1;
    }

    // Читаємо файл рядок за рядком
    string line;
    while (getline(file, line))
    {
        // Розділяємо рядок на поля
        size_t pos = 0;
        string field;
        while ((pos = line.find(",")) != string::npos)
        {
            field = line.substr(0, pos);
            // Виводимо поле у консоль
            cout << field << ",";
            // Пропускаємо кому
            line.erase(0, pos + 1);
        }
        // Виводимо останнє поле та перехід на новий рядок
        cout << line << endl;
    }

    // Закриваємо файл
    file.close();

    return 0;
}
```

Вище наведений код використовує функцію "find()" для пошуку коми як роздільника між полями у рядку. Після розділення рядка, кожне поле виводиться у консоль. Для роботи з іншими типами даних, можна використовувати функцію "stoi()" для перетворення текстового поля у числове, або "stof()" для перетворення на числа з плаваючою точкою.

## Deep Dive

У мові C++ існує багато бібліотек для роботи з CSV-файлами. Найпопулярніші з них - "libcvs" та "Boost.Tokenizer". Вони надають більше можливостей для обробки даних та дозволяють працювати з більш складними форматами CSV, які містять різноманітні символи роздільників.

## Дивіться також

- [Повне керівництво по роботі з CSV в C++](https://www.geeksforgeeks.org/csv-file-management-using-c-c/)
- [Огляд бібліотек для роботи з CSV в мові C++](https://www.codeguru.com/cpp/cpp/cpp_mfc/general/article.php/c12417/Reading-and-Writing-CSV-Files