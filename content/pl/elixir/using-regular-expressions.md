---
title:    "Elixir: Używanie wyrażeń regularnych"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego warto używać wyrażeń regularnych?

Wyrażenia regularne to potężne narzędzie programistyczne, które pozwala na efektywne manipulowanie tekstami. Mogą być wykorzystane do wyszukiwania, filtrowania, a także podmieniania fragmentów tekstu. Jeśli chcesz osiągnąć szybkie i precyzyjne działanie na tekście, wyrażenia regularne są niezastąpione.

## Jak korzystać z wyrażeń regularnych w Elixirze?

Sposób użycia wyrażeń regularnych w Elixirze jest bardzo prosty. Najpierw musimy skorzystać z wbudowanej funkcji `Regex` i przekazać jej jako argument wyrażenie regularne w postaci stringa, wraz z wywołaniem funkcji `Regex.match/2`. Następnie możemy wykorzystać funkcję `Regex.run/2` aby dopasować wyrażenie regularne do tekstu i otrzymać wynik w postaci listy napisów.

```
Elixir
%% Przykład:
regex = ~r/abc/ # zapisanie wyrażenia regularnego
match = Regex.match(regex, "abcxyz") # dopasowanie wyrażenia do tekstu
result = Regex.run(regex, "xyzabc") # wynikiem będzie ["abc"] 
``` 

## Głębsze zagadnienia dotyczące wyrażeń regularnych

Kiedy już opanujemy podstawy korzystania z wyrażeń regularnych, możemy przejść do bardziej zaawansowanych zastosowań. Na przykład, za pomocą metody `Regex.replace/4` można dokonywać podmiany fragmentów tekstu według określonego wyrażenia regularnego. Istnieje również możliwość wykorzystania operatora `=~` w celu sprawdzenia czy dany tekst pasuje do wyrażenia regularnego.

```
Elixir
%% Przykładowe wykorzystanie:
Regex.replace(~r/\d+/, "a1b2c3", "*") # wynikiem jest "a*b*c*"
"a1b2c3" =~ ~r/[a-z]+[0-9]+/ # zwraca true
```

## Zobacz także

- Dokumentacja Elixir Regex: https://hexdocs.pm/elixir/Regex.html
- A tutorial on Elixir Regex: https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm
- Elixir Playground: https://elixirplayground.com/