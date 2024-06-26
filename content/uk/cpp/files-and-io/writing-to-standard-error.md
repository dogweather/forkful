---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:18.994773-07:00
description: "\u042F\u043A: \u0423 C++, \u0437\u0430\u043F\u0438\u0441 \u0443 \u0441\
  \u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0438\u0439 \u043F\u043E\u0442\u0456\
  \u043A \u043F\u043E\u043C\u0438\u043B\u043E\u043A \u043C\u043E\u0436\u043D\u0430\
  \ \u0437\u0434\u0456\u0439\u0441\u043D\u0438\u0442\u0438 \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043F\u043E\u0442\u043E\u043A\u0443\
  \ `cerr`, \u044F\u043A\u0438\u0439 \u0454 \u0447\u0430\u0441\u0442\u0438\u043D\u043E\
  \u044E \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u043E\u0457 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438. \u041E\u0441\u044C \u0431\
  \u0430\u0437\u043E\u0432\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:49.874556-06:00'
model: gpt-4-0125-preview
summary: "\u0423 C++, \u0437\u0430\u043F\u0438\u0441 \u0443 \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442\u043D\u0438\u0439 \u043F\u043E\u0442\u0456\u043A \u043F\
  \u043E\u043C\u0438\u043B\u043E\u043A \u043C\u043E\u0436\u043D\u0430 \u0437\u0434\
  \u0456\u0439\u0441\u043D\u0438\u0442\u0438 \u0437\u0430 \u0434\u043E\u043F\u043E\
  \u043C\u043E\u0433\u043E\u044E \u043F\u043E\u0442\u043E\u043A\u0443 `cerr`, \u044F\
  \u043A\u0438\u0439 \u0454 \u0447\u0430\u0441\u0442\u0438\u043D\u043E\u044E \u0441\
  \u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u043E\u0457 \u0431\u0456\u0431\u043B\
  \u0456\u043E\u0442\u0435\u043A\u0438."
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
weight: 25
---

## Як:
У C++, запис у стандартний потік помилок можна здійснити за допомогою потоку `cerr`, який є частиною стандартної бібліотеки. Ось базовий приклад:

```cpp
#include <iostream>

int main() {
    // Запис у стандартний потік виведення
    std::cout << "Це звичайне повідомлення." << std::endl;
    
    // Запис у стандартний потік помилок
    std::cerr << "Це повідомлення про помилку." << std::endl;
    
    return 0;
}
```

Приклад виведення:
```
Це звичайне повідомлення.
Це повідомлення про помилку.
```

У цьому випадку обидва повідомлення зазвичай з'являться у вашому терміналі, але ви можете спрямовувати їх окремо в оболонці. Наприклад, ви можете надіслати стандартний вивід до файлу, залишаючи помилки для відображення на екрані.

Для більш розширеного реєстрування та обробки помилок можуть бути використані сторонні бібліотеки, як-от `spdlog` або `boost.log`. Ці бібліотеки пропонують розширені функції для реєстрування, включаючи форматування, рівні логів та виведення у файл.

Ось як ви могли б використовувати `spdlog` для запису повідомлення про помилку:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Ініціалізація spdlog
    spdlog::info("Це звичайне повідомлення.");
    spdlog::error("Це повідомлення про помилку.");
    
    return 0;
}
```

Примітка: Щоб використовувати `spdlog`, вам потрібно додати його до свого проекту. Це можна зробити, клонувавши репозиторій з GitHub або використовуючи менеджер пакетів, як-от `vcpkg` або `conan`.

Пам'ятайте, вибір між використанням стандартних потоків безпосередньо або бібліотеки на кшталт `spdlog` залежить від складності вашої аплікації та ваших конкретних потреб стосовно обробки помилок і реєстрування.
