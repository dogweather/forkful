---
title:                "Використання регулярних виразів"
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке & Чому?
Регулярні вирази - це шаблони для пошуку та маніпулювання текстом. Програмісти використовують їх для валідації, парсингу, і розбору рядків - швидко і гнучко.

## Як це робиться:
```elixir
# Знайти всі слова, які починаються з "c"
regex = ~r/c\w*/
string = "Elixir connects concurrency with simplicity."
matches = Regex.scan(regex, string)
IO.inspect(matches) # => [["connects"], ["concurrency"]]

# Замінити всі цифри зірочками
regex = ~r/\d/
string = "Room 404: Resource not found."
new_string = Regex.replace(regex, string, "*")
IO.puts(new_string) # => "Room ***: Resource not found."
```

## Занурення у деталі:
Регулярні вирази запозичені з теорії формальних мов і були популяризовані в Unix утилітах, в 70-их. В Elixir вони вбудовані і використовують бібліотеку регулярних виразів Erlang. Альтернативи включають String.contains?, String.split і String.match?, але вони менш потужні. Під капотом Elixir використовує потужний і оптимізований двигун регулярних виразів, що практично виконує зразки вбудованим кодом.

## Дивіться також:
- [Elixir Regex Docs](https://hexdocs.pm/elixir/Regex.html)
- [Erlang's re module](http://erlang.org/doc/man/re.html)
- [Regular Expressions in Programming](https://en.wikipedia.org/wiki/Regular_expression#History) - історія регулярних виразів.