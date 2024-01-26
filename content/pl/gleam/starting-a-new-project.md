---
title:                "Rozpoczynanie nowego projektu"
date:                  2024-01-20T18:03:41.609874-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zaczynając nowy projekt, tworzysz podwaliny dla swojej aplikacji lub biblioteki. Programiści robią to, aby zorganizować kod od początku i ustalić kierunek rozwoju.

## How to: (Jak to zrobić?)
Aby rozpocząć nowy projekt w Gleam, zainstaluj narzędzie Gleam CLI, a następnie uruchom komendę:

```gleam
gleam new my_cool_project
```

Wynikowe działanie powyższej komendy to stworzenie nowej struktury projektu:

```
my_cool_project/
├── gleam.toml
├── README.md
├── src
│   └── my_cool_project.gleam
└── test
    └── my_cool_project_test.gleam
```

## Deep Dive (Dogłębna analiza)
Gleam, język do tworzenia bezpiecznych, równoległych aplikacji backendowych, zaczął się formować w 2018 roku. Wzorowany na Elixirze i Erlangu, przynosi typowanie statyczne do ekosystemu Erlang VM. Aby zacząć projekt, Gleam oferuje CLI, które ułatwia zarządzanie zależnościami i kompilację.

Alternatywy dla Gleam to Elixir, Erlang czy Elm, ale Gleam wyróżnia się dzięki systemowi typów podobnemu do języków takich jak Rust lub Haskell. Rozpoczęcie projektu z Gleam zapewnia szybkie ustawienie środowiska, śledzenie typów i wydajne wykorzystanie BEAM (Erlang VM).

Szczegółową implementację nowego projektu można znaleźć w dokumentacji Gleam CLI, która tłumaczy, jak zarządzać modułami i pakietami.

## See Also (Zobacz także)
- The Gleam Book: [https://gleam.run/book](https://gleam.run/book)
- GitHub repo: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
