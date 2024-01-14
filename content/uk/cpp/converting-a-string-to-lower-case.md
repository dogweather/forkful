---
title:    "C++: Перетворення рядка у нижній регістр"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Чому
У програмуванні часто виникає необхідність змінити регістр вхідного рядка на нижній. Наприклад, для порівняння двох текстів чи для використання в якості ключа у зберіганні даних. У цих випадках, конвертація рядка до нижнього регістру є корисною функцією.

## Як це зробити
Для зручності, ми розглянемо декілька прикладів з використанням мови програмування C++. За допомогою стандартної функції `tolower()` та циклу можна легко змінити регістр кожного символу у вхідному рядку на нижній:

```C++
// Приклад 1: Звичайний цикл
#include <iostream>
#include <cstring>
#include <cctype>

int main() {
    char str[] = "Прикладний Текст";
    for (int i = 0; i < strlen(str); i++) {
        str[i] = tolower(str[i]);
    }
    std::cout << str; // Результат: прикладний текст
}
```

Однак, у мові C++ існує більш простий спосіб виконати цю задачу, застосувавши алгоритм `std::transform()` та функцію `tolower()`:

```C++
// Приклад 2: Функція transform та tolower
#include <iostream>
#include <algorithm>
#include <string>
#include <cctype>

std::string to_lowercase(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    return str;
}

int main() {
    std::string str = "Прикладний Текст";
    std::cout << to_lowercase(str); // Результат: прикладний текст
}
```

## Глибоке занурення
Більш детально розглянемо алгоритм `std::transform()`. Цей алгоритм приймає три параметри: початковий та кінцевий ітератори та функцію, що визначає операцію над елементами. У нашому випадку, використовується функція `::tolower`, яка змінює регістр символу на нижній. Ще однією корисною якістю цього алгоритму є те, що він може працювати не тільки зі строковими значеннями, а й з будь-якими контейнерами, наприклад, з вектором.

```C++
// Приклад 3: Transform та вектор
#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <cctype>

int main() {
    std::vector<std::string> strs = {"Перша Строка", "Друга Строка", "Третя Строка"};
    std::transform(strs.begin(), strs.end(), strs.begin(), ::tolower);
    for (auto str : strs) {
        std::cout << str << std::endl; 
    }
    // Результат:
    // перша строка
    // друга строка
    // третя строка
}
```

## Дивись також
- [Документація на `std::transform()`](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Документац