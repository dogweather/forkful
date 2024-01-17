---
title:                "Отримання поточної дати"
html_title:           "C#: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що & Зачому?
Отримання поточної дати - це процес, за допомогою якого програмісти можуть отримати поточну дату і час. Це корисна функція, яка допомагає в роботі з датами та часом в програмах.

## Як зробити:
Для отримання поточної дати використовуйте статичний метод `DateTime.Now` з класу `DateTime`. Наприклад:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

При виконанні цього коду ви отримаєте поточну дату та час у форматі `MM/dd/yyyy hh:mm:ss`.

## Глибокий занурення:
Отримання поточної дати в програмуванні використовується для різних цілей, таких як ведення журналів, роботи з датовими об'єктами та встановлення часових меж для виконання певних функцій. Наприклад, у веб-розробці поточна дата може використовуватися для відображення часу останнього оновлення вмісту на сторінці. Крім того, нарівні з `DateTime.Now` є ще кілька методів для отримання поточної дати: `DateTime.Today` та `DateTime.UtcNow`.

## Дивіться також:
Для додаткової інформації щодо роботи з датами та часом в `C#`, дивіться документацію Microsoft про клас `DateTime`: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)