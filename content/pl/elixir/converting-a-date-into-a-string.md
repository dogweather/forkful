---
title:    "Elixir: Konwersja daty na ciąg znaków"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w Elixir może być bardzo przydatne w przypadku konwersji daty na łańcuch znaków. Jest to ważne, ponieważ często musimy przedstawić daty w czytelnej formie dla użytkowników lub dla innych części naszej aplikacji.

## Jak to zrobić

W Elixir możemy wykorzystać wbudowane moduły i funkcje do konwersji daty na łańcuch znaków. Przykład kodu pokazującego to w praktyce wyglądałby następująco:

```Elixir
date = Date.utc_today()
output = Date.to_iso8601(date)
IO.puts(output) # 2022-01-01
```

Ten przykład wykorzystuje funkcję `Date.utc_today()` do utworzenia obiektu daty reprezentującego dziś. Następnie wykorzystujemy funkcję `Date.to_iso8601/1` do przekonwertowania daty na łańcuch znaków w formacie ISO-8601. W tym przypadku używamy funkcji `IO.puts/1` do wyświetlenia wyniku w konsoli.

Możemy również użyć funkcji `DateTime.to_iso8601/1` do konwersji daty i czasu na łańcuch znaków. Przykład:

```Elixir
datetime = DateTime.utc_now()
output = DateTime.to_iso8601(datetime)
IO.puts(output) # 2022-01-01T00:00:00Z
```

W przypadku, gdy chcemy dostosować format wynikowego łańcucha znaków, możemy skorzystać z funkcji `Date.format/2` lub `DateTime.format/2`. Przykład:

```Elixir
date = Date.utc_today()
output = Date.format(date, "{DD}/{MM}/{YYYY}")
IO.puts(output) # 01/01/2022
```

W powyższym przykładzie używamy specjalnych znaków w nawiasach klamrowych, aby określić pożądany format dla funkcji `Date.format/2`.

## Głębsza analiza

Jeśli chcemy lepiej zrozumieć proces konwersji daty na łańcuch znaków, warto przeanalizować jak moduły `Date` i `DateTime` są zdefiniowane w Elixir. Wracając do przykładu z funkcją `Date.to_iso8601/1`, można zauważyć, że ta funkcja jest zdefiniowana w następujący sposób:

```Elixir
@spec to_iso8601(date :: Date.t) :: String.t()
def to_iso8601(date) do
  with {:ok, _tz} <- Timezone.construct(date) do
    date
    |> to_tuple()
    |> to_string()
  end
end
```

Podczas gdy funkcja `Date.utc_today/0` jest zdefiniowana tak:

```Elixir
@spec utc_today() :: Date.t()
def utc_today(), do: Date.utc_from_erl(:calendar.universal_time())
```

Zauważmy, że funkcja `Date.utc_today/0` wykorzystuje funkcję `:calendar.universal_time/0` z biblioteki standardowej Erlanga, aby uzyskać obecny datę uniwersalną.

## Zobacz także

- Dokumentacja Elixir na temat konwersji daty i czasu: https://hexdocs.pm/elixir/Date.html#to_iso8601/1
- Przewodnik po języku Elixir: https://elixir-lang.org/getting-started/introduction.html
- Szybki kurs Elixir na Codecademy: https://www.codecademy.com/learn/learn-elixir