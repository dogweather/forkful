---
title:    "C++: Порівняння двох дат"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Для чого

Порівняння дат є важливою частиною програмування, оскільки дати є необхідними в багатьох сферах, таких як фінанси, логістика та веб-розробка. Порівнювання дат допомагає встановити, яка дата є пізнішою або ранішою, що є важливим для багатьох завдань програмістів.

# Як

Існує декілька способів порівняння дат у С++, але ми розглянемо найбільш простий і ефективний застосування функції `std::chrono::duration`. Вона дозволяє обчислити різницю між датами та використовувати цю інформацію для порівняння дат.

```C++
#include <iostream>
#include <chrono>

int main() {
    // Створення дат
    // Перша дата: 20 січня 2021 року
    std::chrono::system_clock::time_point date1 =
        std::chrono::system_clock::time_point( 
            std::chrono::system_clock::duration(1611126000));
    
    // Друга дата: 5 лютого 2021 року
    std::chrono::system_clock::time_point date2 = 
        std::chrono::system_clock::time_point(
            std::chrono::system_clock::duration(1612542000));
    
    // Обчислення різниці між датами
    std::chrono::duration<int, std::ratio<86400>> diff = 
        date2 - date1;
    
    // Використання значення різниці для порівняння дат
    if (diff.count() > 0) {
        std::cout << "Дата 2 є пізнішою за дату 1." << std::endl;
    } else if (diff.count() < 0) {
        std::cout << "Дата 1 є пізнішою за дату 2." << std::endl;
    } else {
        std::cout << "Дати рівні." << std::endl;
    }
    
    return 0;
}
```

**Вихідні дані:**

```
Дата 2 є пізнішою за дату 1.
```

# Поглиблене дослідження

Існує кілька типів даних у С++, які можна використовувати для представлення дат, наприклад `std::chrono::system_clock::time_point` та `std::chrono::steady_clock::time_point`. Також існують різні методи обчислення різниці між датами, такі як `std::chrono::duration`, `std::chrono::duration_cast` та `std::chrono::time_point_cast`.

Для більш докладної інформації та прикладів використання даних методів рекомендуємо ознайомитися з офіційною документацією С++.

# Див. також

- [Офіційна документація С++](https://en.cppreference.com/w/cpp/chrono)
- [Туторіал з порівняння дат у С++](https://www.learncpp.com/cpp-tutorial/keeping-track-of-time/)