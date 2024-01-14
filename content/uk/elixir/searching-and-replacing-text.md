---
title:                "Elixir: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Навіщо

В програмуванні існує натхнення та важливість для шукання та заміни тексту. Наприклад, ви можете хотіти модифікувати даний рядок тексту до певного формату або замінити деякі символи в даних для зручності обробки.

## Як це зробити

Існує декілька способів шукання та заміни тексту в Elixir. Один зі способів це використовувати функцію `Regex.replace/3` з модуля `Regex`.

```Elixir
# Приклад заміни слова "EEC" на "Elixir" в даних
Regex.replace(~r/[Ee][Ee][C]/, "Elixir", "Welcome to EEC World") 
# Виведе "Welcome to Elixir World"
```

Ще один спосіб це використовувати метод `String.replace/2`:

```Elixir
# Приклад заміни слова "hello" на "привіт" в рядку
String.replace("hello world", "hello", "привіт") 
# Виведе "привіт world"
```

## Глибока знахідка

При шуканні та заміні тексту, можна детальніше підійти до процесу і використати патерни (patterns) замість простих рядків для пошуку. Наприклад, можна використати даний паттерн, щоб замінити номери телефонів в даних на "XXX-XXX-XXXX":

```Elixir
String.replace("Мій номер телефону: 555-123-4567", ~r/[0-9]{3}-[0-9]{3}-[0-9]{4}/, "XXX-XXX-XXXX") 
# Виведе "Мій номер телефону: XXX-XXX-XXXX"
```

Також, можна використовувати регулярні вирази для складніших випадків, наприклад, для визначення гостей вашого сайту та їх посилань.

## Дивіться також

- [Регулярні вирази в Elixir](https://www.longcode.io/elixir-regular-expressions/)
- [Модуль Regex Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Метод String Elixir](https://hexdocs.pm/elixir/String.html)