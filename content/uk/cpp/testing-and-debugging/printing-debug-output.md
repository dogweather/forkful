---
title:                "Виведення налагоджувальної інформації"
aliases:
- /uk/cpp/printing-debug-output/
date:                  2024-01-20T17:52:27.549587-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Debug вивід дозволяє бачити, що відбувається всередині програми під час її виконання. Програмісти використовують це для виявлення помилок та оптимізації коду.

## Як це робити:
C++ має `iostream` для стандартного виводу. Використовуйте `std::cout` для друкування даних.

```C++
#include <iostream>

int main() {
    int bugCount = 99;
    
    // Стандартний дебажний вивід
    std::cout << "Кількість помилок: " << bugCount << std::endl;
    
    // Умовний дебажний вивід
    #ifdef DEBUG
    std::cerr << "Помилка змінної bugCount: " << bugCount << std::endl;
    #endif

    return 0;
}
```
Вивід:
```
Кількість помилок: 99
```

## Поглиблений огляд:
Дебаг в C++ започаткований роки тому, відрізняється простотою. Використання `std::cout` для ведення журналу відомо всім. Альтернативи включають логувальні фреймворки і засоби на зразок `spdlog`. `std::cerr` підходить для помилок та термінових повідомлень, бо відправляє дані прямо в потік помилок. Макрос `DEBUG` можна використати для включення виводу тільки при компіляції з флагом `-DDEBUG`.

## Дивіться також:
- [cppreference std::cout](https://en.cppreference.com/w/cpp/io/cout)
- [spdlog GitHub](https://github.com/gabime/spdlog) 
- [GDB — GNU проект дебагера](https://www.gnu.org/software/gdb/)

**Примітка**: Всі посилання ведуть на англомовні ресурси.
