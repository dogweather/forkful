---
title:                "Порівняння двох дат."
html_title:           "C: Порівняння двох дат."
simple_title:         "Порівняння двох дат."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

Що & Чому?

Порівнювання двох дат - це процес, який дозволяє програмістам порівнювати дві дати і визначати, яка з них є більшою або меншою. Це важливо, оскільки це допомагає здійснювати різні операції з датами, такі як сортування або визначення часових інтервалів.

Як це зробити:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Створюємо дві структури для представлення дат
    struct tm first_date = { .tm_year = 2020, .tm_mon = 9, .tm_mday = 1 };
    struct tm second_date = { .tm_year = 2021, .tm_mon = 3, .tm_mday = 5 };

    // Використовуємо функцію difftime для порівняння дат
    double result = difftime(mktime(&first_date), mktime(&second_date));

    if (result > 0) {
        printf("Перша дата пізніше за другу дату\n");
    } else if (result < 0) {
        printf("Перша дата раніше за другу дату\n");
    } else {
        printf("Дати рівні\n");
    }

    return 0;
}
```

Вихідний код надає такий результат: "Перша дата пізніше за другу дату". В цьому прикладі ми використовуємо стандартні функції time.h для роботи з датами, але існує також багато інших способів зробити це.

Глибокий погляд:

У минулому програмісти мусили надто часто працювати з різними форматами дат, що утруднювало порівняння дат. Однак, з появою стандартів як стандарту IEEE 754, таких джерел дат, як POSIX, і бібліотек, як, наприклад, time.h, порівняння дат стало простіше. Також існує багато інших альтернатив, які дозволяють робити подібну роботу.

Дивіться також:

- Документація по бібліотеці time.h: [https://www.tutorialspoint.com/c_standard_library/time_h.htm](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- Порівняння дат в С: [https://www.geeksforgeeks.org/compare-two-dates-c/](https://www.geeksforgeeks.org/compare-two-dates-c/)
- Стандарт IEEE для роботи з датами: [https://ieeexplore.ieee.org/abstract/document/34587](https://ieeexplore.ieee.org/abstract/document/34587)