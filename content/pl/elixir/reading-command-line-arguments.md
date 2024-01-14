---
title:                "Elixir: Odczytywanie argumentów wiersza poleceń"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego życia. Niezależnie od tego, czy jesteś początkującym programistą czy doświadczonym profesjonalistą, nie możesz uniknąć pisania programów, które wymagają wprowadzenia danych przez użytkownika. Właśnie dlatego warto poznać jak odczytywać argumenty wiersza poleceń w języku Elixir. Pozwala to na większą kontrolę nad naszymi programami i lepsze dostosowanie ich do naszych potrzeb.

## Jak To Zrobić

Aby odczytać argumenty wiersza poleceń w Elixir, możemy użyć modułu `OptionParser`. Najpierw musimy zaimportować ten moduł, a następnie skorzystać z funkcji `parse` w celu analizy argumentów. Przykładowy kod może wyglądać następująco:

```Elixir
import OptionParser

options = OptionParser.parse([
  { "-h", "--help", "Print help message." },
  { "-c", "--code", "Print code.", [required_type: :integer] },
  { "-l", "--lang", "Programming language.", [required_type: :atom] }
])

IO.puts options[:help] # wyświetli true, jeśli użyjemy flagi -h lub --help
IO.puts options[:code] # wyświetli podane przez nas wartości, np. 123
IO.puts options[:lang] # wyświetli podane przez nas wartości, np. :Elixir
```
Przykładowy output po uruchomieniu programu z argumentami `-c 123 -l Elixir` może wyglądać następująco:

```
true
123
:Elixir
```

## Głębsza Analiza

Powyższy przykład jest jedynie prostym wprowadzeniem do odczytania argumentów wiersza poleceń w Elixir. Moduł `OptionParser` oferuje również wiele innych funkcjonalności, takich jak możliwość dodawania wartości domyślnych lub określania wymaganych typów danych. Dokładną dokumentację tego modułu można znaleźć tutaj: [https://hexdocs.pm/elixir/OptionParser.html](https://hexdocs.pm/elixir/OptionParser.html)

## Zobacz także

1. [https://elixir-lang.org/](https://elixir-lang.org/)
2. [https://elixirschool.com/pl/lessons/basics/command-line-args/](https://elixirschool.com/pl/lessons/basics/command-line-args/)
3. [https://medium.com/@gusy_86232/how-to-parse-command-line-arguments-via-shell-pipeline-in-elixir-8f257e2eae5c](https://medium.com/@gusy_86232/how-to-parse-command-line-arguments-via-shell-pipeline-in-elixir-8f257e2eae5c)