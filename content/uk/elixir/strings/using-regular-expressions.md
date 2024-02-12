---
title:                "Використання регулярних виразів"
aliases: - /uk/elixir/using-regular-expressions.md
date:                  2024-02-03T19:16:54.032611-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Регулярні вирази (regex) в Elixir використовуються для пошуку, відповідностей та маніпуляцій з рядками на основі певних шаблонів. Програмісти вдаються до regex для завдань, як-от валідація форматів (електронна пошта, URL-адреси), розбір логів або витягування даних, завдяки його ефективності та універсальності в обробці рядків.

## Як це зробити:

Elixir використовує модуль `Regex`, вдаючись до бібліотеки regex Erlang, для операцій з regex. Ось базові способи використання:

```elixir
# Знаходження збігу з шаблоном - Повертає перший збіг
result_match = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Вивід: ["hello"]

# Пошук усіх збігів
all_matches = Regex.scan(~r/\d/, "Тут є 2 яблука та 5 апельсинів.")
IO.inspect(all_matches) # Вивід: [["2"], ["5"]]

# Заміна частин рядка
replaced_string = Regex.replace(~r/\s+/, "Elixir це весело", "_")
IO.inspect(replaced_string) # Вивід: "Elixir_це_весело"
```

Для більш складних шаблонів та функціоналу можливо вам захочеться скористатися сторонніми бібліотеками, хоча для більшості основних завдань з рядками та зіставленням шаблонів вбудований модуль `Regex` Elixir є досить потужним.

Для виконання пошуку без врахування регістру, використовуйте опцію `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Вивід: ["Hello"]
```

Регулярні вирази можна попередньо скомпілювати для підвищення ефективності при їх багаторазовому використанні:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Вивід: ["hello"]
```

Elixir також підтримує іменовані захвати, що може бути дуже зручно для витягування конкретних частин рядка, одночасно роблячи ваш код більш читабельним:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Вивід: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Цей короткий огляд підкреслює легкість, з якою Elixir обробляє регулярні вирази, дозволяючи використовувати потужні техніки маніпуляції рядками та витягування даних.
