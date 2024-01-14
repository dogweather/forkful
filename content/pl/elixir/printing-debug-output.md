---
title:    "Elixir: Drucken von Debug-Ausgabe"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie wyjścia debugowania jest niezbędnym narzędziem podczas programowania w języku Elixir. Pomaga nam w zrozumieniu działania naszego kodu, identyfikowaniu błędów i poprawianiu go. Jest to szczególnie przydatne w przypadku dużych i złożonych projektów.

## Jak to zrobić

Aby wyświetlić wyjście debugowania w Elixir, możemy użyć funkcji `IO.inspect/2` lub `IO.inspect/3`. Przykładowy kod wyglądałby następująco:

```Elixir
# Użyj funkcji IO.inspect/2
IO.inspect(variable, label: "Zmienna")

# Użyj funkcji IO.inspect/3
IO.inspect(variable, label: "Zmienna", limit: 10)
```

Gdzie zmienna to nazwa zmiennej, którą chcemy sprawdzić, a limit to opcjonalny parametr określający maksymalną ilość danych do wyświetlenia. Możemy również podać etykietę, aby łatwiej zidentyfikować wyjście.

Po uruchomieniu naszego kodu z wykorzystaniem funkcji `IO.inspect`, wyświetlone zostaną informacje o zmiennej, wraz z wartością i typem. Dzięki temu możemy zweryfikować, czy dana zmienna ma oczekiwaną wartość w określonym momencie.

## Głębsze przyjrzenie się

Funkcja `IO.inspect` ma również inne opcje, takie jak `:depth`, która określa maksymalną głębokość wyświetlania zagnieżdżonych struktur danych, oraz `:pretty`, która formatuje dane w bardziej czytelny dla człowieka sposób.

Istnieje również możliwość użycia `require IO` w naszym module lub skrypcie i późniejsze użycie tylko funkcji `inspect` zamiast `IO.inspect`. Użycie tej drugiej opcji może sprawić, że nasz kod będzie bardziej czytelny i zwięzły.

## Zobacz również

- [Dokumentacja Elixir - Moduł IO](https://hexdocs.pm/elixir/IO.html)
- [Przewodnik po języku Elixir - Debugowanie](https://elixir-lang.org/getting-started/debugging.html)
- [Podstawy wyjścia debugowania w Elixir](https://thoughtbot.com/blog/elixir-debugging-101)