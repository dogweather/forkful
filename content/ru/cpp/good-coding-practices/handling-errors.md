---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:00.114926-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: \u0412\
  \u043E\u0442 \u0431\u0430\u0437\u043E\u0432\u044B\u0439 \u0431\u043B\u043E\u043A\
  \ try-catch \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438\
  \ \u0438\u0441\u043A\u043B\u044E\u0447\u0435\u043D\u0438\u044F."
lastmod: '2024-03-13T22:44:45.621419-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0431\u0430\u0437\u043E\u0432\u044B\u0439 \u0431\u043B\
  \u043E\u043A try-catch \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\
  \u043A\u0438 \u0438\u0441\u043A\u043B\u044E\u0447\u0435\u043D\u0438\u044F."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как сделать:
Вот базовый блок try-catch для обработки исключения:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Упс! Что-то пошло не так.");
    } catch (const std::exception& e) {
        std::cerr << "Ошибка: " << e.what() << std::endl;
    }
    return 0;
}
```

Пример вывода:
```
Ошибка: Упс! Что-то пошло не так.
```

## Глубокое погружение
В C++ обработка ошибок существует с самых ранних дней. Самая базовая форма была проверкой возвращаемых значений. Если вы уже сталкивались с этим, вы помните времена до стандартизации: Си с классами и ручной проверкой ошибок.

Затем появились исключения в C++, чтобы дать нам структурированный способ справляться с непредвиденными проблемами. Исключение вызывается с помощью `throw` и перехватывается с помощью `try/catch`.

Часто возникают два типа ошибок: логические ошибки, как неправильный расчет, и ошибки времени выполнения, как обращение к недействительному адресу памяти. Исключения идеально подходят для ошибок времени выполнения. Для логических ошибок часто лучше использовать утверждения или коды ошибок.

Идет постоянное обсуждение о противостоянии исключений и кодов ошибок. Исключения могут быть медленнее и могут привести к сложным потокам управления. Коды ошибок, хотя и быстрее, могут сделать код загроможденным и трудным для поддержки. Это компромисс, поэтому ключевым является понимание вашего конкретного случая.

C++17 ввел `std::optional` и `std::variant`, которые являются альтернативами исключениям. Они полезны для функций, которые могут возвращать или не возвращать действительный результат.

Безопасность исключений может быть еще одной головной болью. Речь идет о гарантиях, которые ваш код предоставляет несмотря на исключения. Существует три уровня: базовый, сильный и nothrow. Чем больше гарантий, тем сложнее может быть ваш код.

Заключительные мысли — обработка ошибок это столько же искусство, сколько и наука. Она формирует, как ваше приложение выживает в реальных условиях. Не злоупотребляйте исключениями. Стремитесь к читаемому, поддерживаемому коду.

## Смотрите также
- [cppreference по обработке исключений](https://en.cppreference.com/w/cpp/language/exceptions)
- [Мнение Бьярне Страуструпа об обработке ошибок](http://www.stroustrup.com/except.pdf)
- [Рекомендации по C++ Core по исключениям](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
