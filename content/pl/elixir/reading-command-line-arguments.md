---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Elixir: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wczytywanie argumentów wiersza poleceń jest procesem, w którym programista pobiera dane podane przez użytkownika przy uruchamianiu programu. Jest to przydatne, ponieważ pozwala na dostarczenie programowi konkretnych informacji lub opcji, co pozwala na bardziej spersonalizowane użycie programu.

## Jak to zrobić:

Aby wczytać argumenty wiersza poleceń w Elixir, należy użyć funkcji `System.argv`. Przykładowy kod wraz z oczekiwanym wyjściem wygląda następująco:

```Elixir
# Przykładowy kod
args = System.argv

# Wyjście
["program_name", "arg1", "arg2"]
```

## Coś więcej:

### Kontekst historyczny:

W przeszłości, czytanie argumentów wiersza poleceń było jedynym sposobem na dostarczenie danych do programów. Jednak w dzisiejszych czasach istnieje wiele alternatywnych sposobów, takich jak wykorzystanie interfejsu użytkownika, aby umożliwić użytkownikom konfigurowanie opcji.

### Alternatywy:

Alternatywne metody dostarczania danych do programów mogą obejmować użycie plików konfiguracyjnych lub interfejsów użytkownika. Każda z tych metod ma swoje zalety i może być bardziej odpowiednia w zależności od danego zadania.

### Szczegóły implementacji:

W Elixir, argumenty wiersza poleceń są dostępne poprzez wywołanie funkcji `System.argv`. Funkcja ta zwraca listę zawierającą nazwę programu i wszystkie podane argumenty. Można również użyć opcji wiersza poleceń do specyfikowania konkretnych argumentów lub ich kolejności.

## Zobacz także:

Niektóre przydatne źródła dotyczące wczytywania argumentów wiersza poleceń w Elixir:

- Dokumentacja Elixir: https://hexdocs.pm/elixir/System.html#argv/0
- Wprowadzenie do Elixir: https://elixir-lang.org/getting-started/modules.html#command-line-arguments
- Wideo na YouTube: https://www.youtube.com/watch?v=3ZLcyR5l0aA