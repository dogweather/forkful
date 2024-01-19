---
title:                "Інтерполяція рядка"
html_title:           "Elixir: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?

Перетворення рядків - це процес, коли ми вбираємо значення змінної в рядок. Прикладом може бути використання ім'я користувача у повідомленні. Інтерполяція рядків дозволяє нам створювати більш динамічні та спростовані рядки в нашому програмуванні.

Як це зробити?

```Elixir
им'я_користувача = "Оля"
повідомлення = "Привіт, #{ім'я_користувача}! Як справи?"
```

Вивід: Привіт, Оля! Як справи?

Глибокий пірнання

Історичний контекст:
Інтерполяція рядків була вперше використана у мові Perl. У сучасному світі його можна знайти у багатьох інших мов програмування, таких як Ruby, PHP та Java. У Elixir строкова інтерполяція - це вбудована функція, яка дозволяє нам легко збирати рядки змінних.

Альтернативи:
У деяких мовах програмування використовуються конкатенація рядків або форматування рядків для досягнення схожого ефекту. Проте інтерполяція є більш зручною, оскільки не вимагає додаткових операцій і повертає зручний та приємний для ока код.

Деталі реалізації:
Унікальність Elixir полягає у тому, що інтерполяція рядків не тільки обробляє змінні, але також може виконувати вирази. Це дозволяє нам виконувати складні операції при створенні рядків без необхідності використовувати додаткові функції.

Дивись також:
Додаткову інформацію про інтерполяцію рядків у Elixir можна знайти в [документації](https://hexdocs.pm/elixir/String.html#interpolation). Також це може бути корисно порівняти інтерполяцію рядків з конкатенацією рядків та форматуванням рядків у [реальних прикладах](https://elixirschool.com/ru/lessons/basics/string-interpolation/#examples).