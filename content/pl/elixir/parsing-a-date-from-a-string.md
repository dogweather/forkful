---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z ciągu znaków to proces przekształcenia tekstowych reprezentacji daty i czasu w użyteczny format daty, który może być łatwo manipulowany i interpretowany przez programy. Programiści robią to, aby umożliwić oprogramowaniu zrozumienie dat i czasów w różnych formatach i językach.

## Jak to zrobić:
Parsowanie daty w Elixir jest proste. Wystarczy skorzystać z funkcji `Date.from_iso8601/1`.

```elixir
{:ok, data} = Date.from_iso8601("2021-09-29")
IO.puts(data)
```

Wynikiem tego kodu będzie wydrukiem:
```elixir
~D[2021-09-29]
```
Założyliśmy, że data jest w formacie ISO 8601, jeśli jest w innym formacie, musisz to przekształcić odpowiednio, używając funkcji `Date.parse/2`.
```elixir
{:ok, data} = Date.parse("29/09/2021", "{0D}/{1M}/{2YYYY}")
IO.puts(data)
```

Wynikiem tego kodu będzie wydrukiem:
```elixir
~D[2021-09-29]
```
## Dogłębne zrozumienie
Elixir, będąc funkcjonalnym językiem programowania, pozwala na eleganckie i proste parsowanie dat. Wersje starsze od 1.3 nie zawierały wbudowanej obsługi daty, co skłoniło twórców do dodania module `Date` w wersji 1.3.

Istnieje kilka alternatyw do parsowania dat, takich jak skorzystanie z pakietu zewnętrznego. Pakiety takie jak `Timex` lub `Calendar` dostarczają szereg funkcji do obsługi dat, czasów i stref czasowych. 

Implementacja Elixir wykorzystuje mechanizm `Calendar.ISO`, który jest domyślnym kalendarzem w Elixir, do przekształcenia ciągu znaków ISO 8601 w datę. Moduły do parsowania dat są sprytnie projektowane, pozwalają na łatwe dodawanie obsługi innych formatów dat, zgodnie z potrzebami programistów.

## Zobacz także
1. Elixir Date: https://hexdocs.pm/elixir/Date.html
2. Elixir Date from_iso8601: https://hexdocs.pm/elixir/Date.html#from_iso8601/2
3. Elixir Date parse: https://hexdocs.pm/elixir/Date.html#parse/2
4. Pakiet Timex: https://hexdocs.pm/timex/readme.html
5. Pakiet Calendar: https://hexdocs.pm/calendar/readme.html