---
title:    "Elixir: Отримання поточної дати"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Чому

Один з найбільш поширених завдань, що стикаються програмісти, це отримання поточної дати. Це може бути корисно для створення зручних інтерфейсів для користувачів, контролю часу виконання коду або для відстеження подій із хронології. В Еліксирі є кілька способів отримання поточної дати, і ми розглянемо їх у цій статті.

## Як

Найпростішим способом отримати поточну дату є використання вбудованої функції `DateTime.now()`. Ця функція повертає об'єкт DateTime, що містить поточну дату і час. Наприклад:

```Elixir
DateTime.now()
# Результат: {:ok, ~U[2021-11-22 09:30:00Z]}
```

Зверніть увагу, що `~U[...]` - це літерал дати і часу в Еліксирі. Ми також можемо отримати поточний час в окремому форматі, використовуючи функцію `DateTime.to_iso8601()`:

```Elixir
DateTime.to_iso8601(DateTime.now())
# Результат: {:ok, "2021-11-22T09:30:00Z"}
```

Якщо вам потрібна лише дата без часу, ви можете використовувати функцію `Date.utc_today()` аналогічним чином.

```Elixir
Date.utc_today()
# Результат: {:ok, ~D[2021-11-22]}
```

## Глибокий дайв

Якщо ви технічно поглянете на реалізацію функцій, які ми використовуємо для отримання поточного часу, ви можете побачити, що вони використовують модуль `:calendar` стандартної бібліотеки Еліксира. Цей модуль містить функції для роботи з датами, часами, таймзонами та багато іншого. Всі наші функції, що отримують поточний час, використовують функцію `:calendar.universal_time()` з параметрами `{0,0,0}` для дати та часу, що означає поточну дату та час у всесвітньому часі. Як результат, наші функції повертають однакові значення.

## Дивіться також

- [Вбудовані функції Еліксира](https://elixir-lang.org/getting-started/basic-types.html#built-in-functions)
- [Модуль Calendar](https://hexdocs.pm/elixir/Calendar.html)