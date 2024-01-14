---
title:    "C++: Читання аргументів командного рядка"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Чому

Програмісти часто використовують командний рядок для передачі аргументів до їх програм. Це дозволяє запускати програму з різними параметрами без необхідності змінювати код. Читання командних аргументів дозволяє робити програми більш гнучкими та зручними для використання.

## Як це зробити

Для читання командних аргументів використовується об'єкт типу `argv` в C++. Розглянемо приклад коду, який можна використовувати для читання аргументів та виведення їх значень:

```C++
#include <iostream>

int main(int argc, char *argv[]) {
    // Виведення кількості аргументів
    std::cout << "Кількість аргументів: " << argc << "\n";

    // Виведення значень аргументів
    for (int i = 0; i < argc; i++) {
        std::cout << "Аргумент " << i << ": " << argv[i] << "\n";
    }
    return 0;
}
```

При запуску програми з аргументами `./program arg1 arg2`, виходом буде:

```
Кількість аргументів: 3
Аргумент 0: ./program
Аргумент 1: arg1
Аргумент 2: arg2
```

## Занурення у глибини

Цей приклад є простою демонстрацією того, як можна читати та використовувати аргументи з командного рядка. Однак, існують і більш складні сценарії, наприклад, обробка різних типів аргументів, опції та флагів. Також слід звернути увагу на валідацію та обробку помилок при читанні аргументів. Для більш детального розгляду цих питань, рекомендуємо дослідити різні методики та кращі практики читання командних аргументів в C++.

## Дивіться також

- [Підручник з читання аргументів командного рядка в C++](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)
- [Стаття про обробку аргументів командного рядка в C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Порівняння різних бібліотек для роботи з аргументами командного рядка в C++](https://github.com/jarro2783/cxxopts/wiki/Comparison-to-other-libraries)