---
title:    "Elixir: Пошук та заміна тексту"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні часто виникає потреба замінювати текст у великій кількості файлів або документів. Це може призвести до великих затрат часу та зусиль. Використовуючи Elixir, ми можемо ефективно та легко здійснити пошук та заміну тексту за допомогою вбудованих функцій.

## Як це зробити

Для початку, встановимо Elixir та iex (інтерактивна оболонка Elixir) на нашому комп'ютері. Після цього, створимо новий файл з будь-яким текстом, який ми хочемо змінити.

```Elixir
# Ми можемо скористатись функцією File.read для читання тексту з файлу
text = File.read("sample.txt")
# Далі, використовуючи функцію String.replace, замінимо слово "привіт" на "добрий день"
new_text = String.replace(text, "привіт", "добрий день")
# Нарешті, за допомогою функції File.write, запишемо новий текст у файл
File.write("sample.txt", new_text)
# Тепер, якщо дістанемося до файлу, побачимо, що слово "привіт" було замінено на "добрий день"
```

## Вдосконалюємо наш код

Якщо ми хочемо бути ще більш гнучкими, можемо використати регулярні вирази для пошуку і заміни тексту.

```Elixir
# Наприклад, ми хочемо замінити всі слова "привіт" на "hello"
new_text = String.replace(text, ~r/привіт/, "hello")
```

Крім того, Elixir дозволяє використовувати функцію String.replace для заміни більш складних частин тексту, наприклад HTML-тегів.

```Elixir
# Наприклад, замінимо всі h1 теги на h2
new_text = String.replace(text, ~r/<h1>/, "<h2>")
```

## Глибоке погруження

Elixir надає багато корисних функцій для пошуку та заміни тексту, таких як String.replace, String.replace_leading, String.replace_trailing та багато інших. Ми також можемо використовувати функцію String.replace для роботи зі списками та мапами.

Детальну інформацію про всі можливості пошуку та заміни тексту у Elixir можна знайти у [документації](https://hexdocs.pm/elixir/String.html#replace/3) та [офіційному посібнику](https://elixir-lang.org/getting-started/string-patterns-and-regular-expressions.html).

## Дивись також

- [Регулярні вирази у Elixir](https://elixir-lang.org/getting-started/pattern-matching.html#regular-expressions)
- [Офіційний сайт Elixir](https://elixir-lang.org/)
- [Курси з Elixir на Codeacademy](https://www.codecademy.com/learn/