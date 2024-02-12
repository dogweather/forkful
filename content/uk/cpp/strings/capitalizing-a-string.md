---
title:                "Зробити першу літеру рядка великою"
aliases:
- /uk/cpp/capitalizing-a-string/
date:                  2024-02-03T19:05:27.978954-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Перетворення рядка на заголовний регістр передбачає перетворення початкового символа кожного слова у рядку на велику літеру, якщо він у нижньому регістрі, залишаючи решту символів незмінними. Програмісти часто виконують це завдання для форматування виводів, введень користувачів або обробки даних, щоб забезпечити узгодженість у представленні або обробці тексту, особливо в користувацьких інтерфейсах або завданнях нормалізації даних.

## Як:
У C++, ви можете зробити рядок заголовним за допомогою стандартної бібліотеки без необхідності використання сторонніх бібліотек. Однак, для більш складних або специфічних поведінок перетворення на заголовний регістр, бібліотеки, як-от Boost, можуть бути дуже корисними. Нижче наведено приклади, що ілюструють обидва підходи.

### Використання стандартної бібліотеки C++:

```cpp
#include <iostream>
#include <cctype> // для std::tolower та std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Результат: "Hello World From C++"
}
```

### Використання бібліотеки Boost:

Для більш складної маніпуляції з рядками, включаючи чутливе до локалі перетворення на заголовний регістр, вам може знадобитися використовувати бібліотеку Boost String Algo.

Спочатку переконайтеся, що у вашому проєкті встановлено та налаштовано бібліотеку Boost. Після цього можна підключити необхідні заголовочні файли та використовувати її можливості, як показано нижче.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // перетворює першу літеру кожного слова на велику
    boost::algorithm::to_lower(capitalizedText); // переконуємося, що рядок у нижньому регістрі
    capitalizedText[0] = std::toupper(capitalizedText[0]); // робимо перший символ великою літерою

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // робимо велику літеру після пробілу
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Результат: "Hello World From C++"
}
```

У цьому випадку, Boost спрощує деякі завдання маніпуляції з рядками, але все ж вимагає спеціального підходу для справжнього перетворення на заголовний регістр, оскільки в основному пропонує утиліти для трансформації та конвертації регістру.
