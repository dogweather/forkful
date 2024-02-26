---
date: 2024-01-26 04:39:35.120828-07:00
description: "Liczby zespolone maj\u0105 cz\u0119\u015B\u0107 rzeczywist\u0105 i cz\u0119\
  \u015B\u0107 urojon\u0105 (takie jak `3 + 4i`). S\u0105 u\u017Cywane w in\u017C\
  ynierii, fizyce i w pewnych problemach obliczeniowych.\u2026"
lastmod: '2024-02-25T18:49:33.457625-07:00'
model: gpt-4-0125-preview
summary: "Liczby zespolone maj\u0105 cz\u0119\u015B\u0107 rzeczywist\u0105 i cz\u0119\
  \u015B\u0107 urojon\u0105 (takie jak `3 + 4i`). S\u0105 u\u017Cywane w in\u017C\
  ynierii, fizyce i w pewnych problemach obliczeniowych.\u2026"
title: Praca z liczbami zespolonymi
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone mają część rzeczywistą i część urojoną (takie jak `3 + 4i`). Są używane w inżynierii, fizyce i w pewnych problemach obliczeniowych. Programiści pracują z nimi przy symulacjach, przetwarzaniu sygnałów oraz rozwiązywaniu pewnych typów problemów matematycznych efektywnie.

## Jak to zrobić:
Elixir nie posiada wbudowanych typów liczbowych zespolonych, więc musimy stworzyć własne lub skorzystać z biblioteki, takiej jak `ComplexNum`. Oto szybki przykład z użyciem biblioteki:

```elixir
# Zakładając, że masz zainstalowanego ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def dodaj(a, b) do
    ComplexNum.add(a, b)
  end
end

# Tworzenie liczb zespolonych i ich dodawanie
c1 = {3, 4}   # reprezentuje 3 + 4i
c2 = {2, -3}  # reprezentuje 2 - 3i
wynik = ComplexMath.dodaj(c1, c2)
IO.puts "Wynik to: #{inspect(wynik)}"
```

To zwróci:
```
Wynik to: {5, 1}
```

Oznacza to, że suma `3 + 4i` i `2 - 3i` to `5 + 1i`.

## Pogłębiona analiza
Liczby zespolone pojawiły się w historii, ponieważ zwykłe liczby nie mogły poradzić sobie z pierwiastkami kwadratowymi z liczb ujemnych. Dopiero w XVII wieku zaczęto je traktować poważnie, dzięki matematykom takim jak René Descartes i Gerolamo Cardano.

W Elixirze często używa się krotek typu `{3, 4}` dla liczb zespolonych, lub korzysta z dedykowanej biblioteki, by uniknąć wynajdywania koła na nowo. Biblioteki są zazwyczaj lepsze - radzą sobie z trudnościami takimi jak mnożenie i dzielenie, które stają się skomplikowane z powodu jednostki urojonej 'i' (dla informacji: kwadrat `i` równa się `-1`).

## Zobacz również
Sprawdź te zasoby:
- [Biblioteka ComplexNum](https://hex.pm/packages/complex_num) dla menedżera pakietów Elixira, Hex.
- [Elixir School](https://elixirschool.com/en/), dla zaawansowanych tematów i ćwiczeń z Elixira.
- [Erlang -- moduł matematyczny](http://erlang.org/doc/man/math.html), którego Elixir używa pod spodem, dla innych potrzeb matematycznych.
