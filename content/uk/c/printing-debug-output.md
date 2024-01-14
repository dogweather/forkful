---
title:    "C: Виведення відладочного виводу"
keywords: ["C"]
---

{{< edit_this_page >}}

## Чому

Натиснення відлагодження — це необхідна частина процесу програмування, яка допомагає знайти та виправити помилки у коді. Подрук відлагодження дозволяє переглядати значення змінних, що допомагає зрозуміти, яким чином програма працює та які кроки потрібно зробити для вдосконалення її функціональності.

## Як це зробити

Для друку відлагодження використовуються функції printf () та fprintf (), які дозволяють виводити значення змінних у консоль або у вказаний файл. Нижче наведено приклади використання цих функцій у мові С:

```C
#include <stdio.h>

int main()
{
    int a = 10;
    float b = 3.14;
    char c = 'D';

    // друкуємо значення змінних у консоль
    printf("Змінна a має значення %d \n", a);
    printf("Змінна b має значення %f \n", b);
    printf("Змінна c має значення %c \n", c);

    // друкуємо значення змінних у файл output.txt
    FILE *file = fopen("output.txt", "w");
    fprintf(file, "Змінна a має значення %d \n", a);
    fprintf(file, "Змінна b має значення %f \n", b);
    fprintf(file, "Змінна c має значення %c \n", c);
    fclose(file);

    return 0;
}
```

Результат виконання програми буде наступним:

```
Змінна a має значення 10 
Змінна b має значення 3.140000 
Змінна c має значення D 
```

Значення змінних також будуть записані у файл output.txt.

## Поглиблене дослідження

У деяких випадках для відлагодження потрібно докладніша інформація про внутрішні процеси програми. У такому разі можна використовувати додаткові функції, наприклад, sprintf (), яка дозволяє форматувати рядки зі значеннями змінних та записувати їх у змінну. Також для більш зручного відладки можна використовувати умовні оператори для виводу певних повідомлень чи значень змінних.

## Дивись також

- [Відлагодження у мові С](https://www.gnu.org/software/gdb/)
- [Документація з функції printf ()](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)
- [Документація з функції fprintf ()](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)