---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:54.032611-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Elixir \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\
  \ \u043C\u043E\u0434\u0443\u043B\u044C `Regex`, \u0432\u0434\u0430\u044E\u0447\u0438\
  \u0441\u044C \u0434\u043E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0438 regex Erlang, \u0434\u043B\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0456\
  \u0439 \u0437 regex. \u041E\u0441\u044C \u0431\u0430\u0437\u043E\u0432\u0456 \u0441\
  \u043F\u043E\u0441\u043E\u0431\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u0430\u043D\u043D\u044F."
lastmod: '2024-03-13T22:44:48.706623-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454 \u043C\u043E\u0434\u0443\u043B\u044C `Regex`, \u0432\u0434\u0430\u044E\u0447\
  \u0438\u0441\u044C \u0434\u043E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0438 regex Erlang, \u0434\u043B\u044F \u043E\u043F\u0435\u0440\u0430\u0446\
  \u0456\u0439 \u0437 regex."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

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
