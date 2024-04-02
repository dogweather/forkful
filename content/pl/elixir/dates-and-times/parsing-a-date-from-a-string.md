---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:16.016190-07:00
description: "Parsowanie daty z ci\u0105gu znak\xF3w polega na przekszta\u0142ceniu\
  \ tekstu, takiego jak \"2023-04-05\", na format daty, kt\xF3ry Tw\xF3j program mo\u017C\
  e zrozumie\u0107 i z kt\xF3rym\u2026"
lastmod: '2024-03-13T22:44:35.053238-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie daty z ci\u0105gu znak\xF3w polega na przekszta\u0142ceniu tekstu,\
  \ takiego jak \"2023-04-05\", na format daty, kt\xF3ry Tw\xF3j program mo\u017C\
  e zrozumie\u0107 i z kt\xF3rym\u2026"
title: "Analiza sk\u0142adniowa daty z ci\u0105gu znak\xF3w"
weight: 30
---

## Co i dlaczego?

Parsowanie daty z ciągu znaków polega na przekształceniu tekstu, takiego jak "2023-04-05", na format daty, który Twój program może zrozumieć i z którym może pracować. Programiści robią to, ponieważ daty występują w wielu formatach i potrzebują spójności, aby móc je porównywać, sortować lub prawidłowo przechowywać.

## Jak to zrobić:

W Elixirze możesz parsować daty przy pomocy modułu `Date`. Oto jak zamienić ciąg znaków na datę:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Przykładowy wynik:

```elixir
~D[2023-04-05]
```

Aby obsłużyć różne formaty, możesz użyć biblioteki `Timex`:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Przykładowy wynik:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Dogłębna analiza

Funkcja `Date.from_iso8601/1` jest częścią standardowej biblioteki Elixir, wprowadzoną w celu ułatwienia parsowania daty zgodnej ze standardem ISO8601 - powszechnego formatu daty. Ale życie nie jest takie proste; daty występują w mnóstwie formatów. Tu z pomocą przychodzi `Timex`, biblioteka stron trzecich dla Elixir, która jest bogatsza niż wbudowane funkcje daty w Elixirze i pomaga obsłużyć szeroką gamę formatów dat.

Elixir jest niezmienny, co oznacza, że sparsowane daty nie są wyjątkiem; nie mogą być zmieniane po utworzeniu. Ta cecha wiąże się z korzeniami programowania funkcjonalnego Elixir, gwarantując przewidywalność i łatwiejsze debugowanie.

Historycznie rzecz biorąc, parsowanie dat było trudne ze względu na różnorodne standardy. Jednak dzięki bibliotekom takim jak `Timex` i funkcjom języka w Elixir, złożoność ta jest abstrahowana, czyniąc życie programisty odrobinę prostszym.

## Zobacz również

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Dokumentacja Timex](https://hexdocs.pm/timex/Timex.html)
- [Standard ISO8601](https://www.iso.org/iso-8601-date-and-time-format.html)
