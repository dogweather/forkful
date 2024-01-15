---
title:                "Пошук та заміна тексту"
html_title:           "Elixir: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому
Заміна тексту - це часта задача при програмуванні. Використовуючи Еліксир, ви можете легко замінити текст у файлах та змінних, що полегшує виконання цієї задачі та забезпечує більш ефективну роботу.

## Як
Існує кілька способів заміни тексту в Еліксирі. Перший - використовувати функцію `String.replace/4` для заміни зі зміною регістру. Наприклад:

```elixir
String.replace("Hello, World!", "hello", "Hi", case: :insensitive)
# Output: "Hi, World!"
```

Другий спосіб - використовувати регулярні вирази з функцією `Regex.replace/3` для заміни з регістрем. Наприклад:

```elixir
Regex.replace(~r/Hello, (\w+)!/, "Hi, $1!", "Hello, John!")
# Output: "Hi, John!"
```

Третій спосіб - використовувати функцію `String.replace/3` для заміни без зміни регістру. Наприклад:

```elixir
String.replace("Hello, World!", "hello", "Hi")
# Output: "Hi, World!"
```

## Глибше
У Еліксирі також існують більш потужні інструменти для заміни тексту, такі як модуль `Stream` та функції `Enum.map/2` та `Enum.reduce/3`. Вони дозволяють вам замінювати текст у великих файлах та списків швидко та ефективно.

Також, Еліксир має вбудований пакет для заміни тексту - `String.Replace`. Цей пакет пропонує різні функції для заміни, включаючи `replace_first/3`, `replace_last/3` та `replace_every/3`. Ви можете використовувати їх для заміни конкретних входжень або всіх входжень в тексті.

## Дивись також
- [Документація Еліксир](https://elixir-lang.org/docs.html)
- [Tutorial для початківців з Еліксиром](https://elixir-lang.org/getting-started/introduction.html)
- [Пакет String.Replace](https://hexdocs.pm/elixir/String.Replace.html)