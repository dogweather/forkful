---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konkatenacja stringów to po prostu ich złączanie, co oznacza połączenie dwóch lub więcej łańcuchów znaków w jeden. Programiści robią to, aby utworzyć dłuższe i bardziej złożone wiadomości lub dane bez konieczności tworzenia wielu zmiennych.

## Jak to zrobić:

Na początek, zainstaluj Elixira za pomocą dowolnego menedżera pakietów lub ze strony domowej Elixira. Potem tworzymy nowy plik .exs i zaczynamy kodować:

```Elixir
string1 = "Witaj, "
string2 = "świecie!"
IO.puts string1 <> string2
```

Po uruchomieniu powyższego kodu, otrzymamy na wyjściu:

```Elixir
"Witaj, świecie!"
```

## Dogłębne informacje:

Z racji tego, że Elixir to język, który powstał w 2011 roku, nie posiada on historycznej roli w kwestii operacji konkatenacji stringów. Niemniej jednak, warto pamiętać, że poza operatorem `<>`, możemy również skorzystać z funkcji `String.concat/2`:

```Elixir
IO.puts String.concat("Witaj, ", "świecie!")
```

Która produkuje ten sam efekt. Warte odnotowania jest również to, że stringi w Elixirze są przechowywane jako listy kodów Unicode, a nie jako proste ciągi znaków, co ma wpływ na wydajność operacji na stringach.

## Zobacz również:

1. Oficjalna dokumentacja Elixira: https://elixir-lang.org/docs.html
2. Elixir School, sekcja poświęcona operacjom na stringach: https://elixirschool.com/pl/lessons/basics/strings/
3. Wprowadzenie do Elixira na stronie Hexdocs: https://hexdocs.pm/elixir/1.12/Kernel.html