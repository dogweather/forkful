---
title:                "Używanie wyrażeń regularnych"
html_title:           "Elixir: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub zainteresowanym nauką programowania, zapewne już słyszałeś o wyrażeniach regularnych, zwanych także regexami. Dzięki nim możemy efektywnie przetwarzać tekst i znalezienie określonego wzoru w długim ciągu znaków staje się prostsze. W artykule tym dowiesz się, dlaczego warto nauczyć się regexów i jak możesz zacząć używać ich w swoich projektach.

## Jak to zrobić

Gotowy do zanurzenia się w świat wyrażeń regularnych? Oto kilka przykładów kodu, które wyjaśnią, jak łatwo jest używać wyrażeń regularnych w Elixirze.

### Wyszukiwanie wzorców

```Elixir
string = "Przykładowy tekst z liczbą 123"

Regex.run(~r/\d+/, string)
# wynik: ["123"]
```

### Zamiana tekstu

```Elixir
string = "Witaj, świecie!"

Regex.replace(~r/świecie/, string, "Elderis")
# wynik: "Witaj, Elderis!"
```

### Walidacja adresu email

```Elixir
email = "janek@przyklad.com"

Regex.match?(~r/\A[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\Z/i, email)
# wynik: true
```

Jak widać, regexy są bardzo elastyczne i można je wykorzystać w różnych celach, od prostych wyszukiwań po złożone walidacje.

## Głębsza perspektywa

Wyrażenia regularne są często wykorzystywane w programowaniu, ponieważ pozwalają na szybkie i precyzyjne przetwarzanie tekstów. Jednak warto pamiętać, że wyrażenia regularne nie są przeznaczone do wszystkiego. Często lepszym wyborem jest użycie funkcji łacińskich lub manipulowanie stringami w języku Elixir.

Podstawowe konstrukcje wyrażeń regularnych warto poznać na pamięć, ponieważ pomogą one w walce z problemami związanymi z przetwarzaniem znaków. Jednak jeśli chcesz pogłębić swoją wiedzę na temat wyrażeń regularnych, możesz zapoznać się z zaawansowanymi konstrukcjami i ich zastosowaniami, takimi jak grupowanie i przechwytywanie danych.

## Zobacz też

- [Dokumentacja Elixir](https://elixir-lang.org/docs.html)
- [Regexr - narzędzie do testowania wyrażeń regularnych](https://regexr.com/)
- [Gotowy zestaw wyrażeń regularnych w Elixirze](http://www.elixirpatterns.com/regular-expressions/)
- [Interaktywny tutorial do nauki wyrażeń regularnych w Elixirze](https://www.alchemistcamp.com/blog/elixir-regular-expressions-cheat-sheet)