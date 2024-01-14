---
title:                "Elixir: Wielkimi literami ciąg znaków"
simple_title:         "Wielkimi literami ciąg znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w trakcie pisania kodu, musimy zmienić sposób wyświetlania tekstu. Może być to zwykłe podkreślenie pewnych wyrazów lub cały ciąg znaków wypisany wielkimi literami. W tym wpisie dowiesz się, dlaczego warto umieć zamieniać pierwszą literę słowa na dużą w języku Elixir.

## Jak to zrobić

Aby zamienić pierwszą literę wyrazu na dużą w Elixirze, musimy użyć funkcji `String.capitalize/1`. Przyjmuje ona jeden argument - tekst, na którym chcemy przeprowadzić operację. W poniższym przykładzie stworzymy zmienną `tekst` zawierającą wyraz "programowanie", a następnie wywołamy funkcję `String.capitalize/1` na tej zmiennej. Wynik operacji zostanie przypisany do nowej zmiennej `tekst_zmieniony`.

```Elixir
tekst = "programowanie"
tekst_zmieniony = String.capitalize(tekst)
IO.puts(tekst_zmieniony)
```

Przykładowy output:

```Elixir
Programowanie
```

Możemy również wywołać funkcję `String.capitalize/1` bezpośrednio na wyrazie, bez tworzenia dodatkowej zmiennej.

```Elixir
IO.puts(String.capitalize("kodowanie"))
```
Przykładowy output:

```Elixir
Kodowanie
```

## Deep Dive

Funkcja `String.capitalize/1` wykorzystuje mechanizm Unicode w celu określenia, która litera powinna być zamieniona na dużą. Dzięki temu jest ona w stanie obsłużyć nie tylko tekst w języku angielskim, ale także w innych językach, które wykorzystują specjalne znaki. Warto również zauważyć, że funkcja ta nie tylko zamienia pierwszą literę na dużą, ale też konwertuje pozostałe litery na małe.

## Zobacz również

- Dokumentacja funkcji `String.capitalize/1`: https://hexdocs.pm/elixir/String.html#capitalize/2
- Wprowadzenie do języka Elixir: https://medium.com/frontendista/poznajemy-elixir-na-podstawie-dziwnej-przygody-kuby-s-b6681a8bdee5
- Przykłady zastosowania funkcji `String.capitalize/1`: https://blog.appsignal.com/2020/08/12/elixir-alphabet-beautiful-toolbox.html#string-capitalize

Dzięki umiejętności zmiany pierwszej litery słowa na dużą, możesz w łatwy sposób dostosować swoje wyświetlane teksty do potrzeb lub wymagań swojego projektu. Nie zapomnij jednak, że funkcja ta działa tylko na pojedynczych słowach - jeśli chcesz zamienić pierwszą literę na dużą w całym zdaniu, musisz podzielić je na pojedyncze wyrazy i na każdym z nich wywołać funkcję `String.capitalize/1`.

## Zobacz również

- Dokumentacja funkcji `String.capitalize/1`: https://hexdocs.pm/elixir/String.html#capitalize/2
- Wprowadzenie do języka Elixir: https://medium.com/frontendista/poznajemy-elixir-na-podstawie-dziwnej-przygody-kuby-s-b6681a8bdee5
- Przykłady zastosowania funkcji `String.capitalize/1`: https://blog.appsignal.com/2020/08/12/elixir-alphabet-beautiful-toolbox.html#string-capitalize