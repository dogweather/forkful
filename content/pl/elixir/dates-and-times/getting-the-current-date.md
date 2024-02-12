---
title:                "Pobieranie aktualnej daty"
aliases: - /pl/elixir/getting-the-current-date.md
date:                  2024-02-03T19:09:30.203696-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie aktualnej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
  - 2024-02-05, dogweather, reviewed and corrected
lastmod:              2024-02-05
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie bieżącej daty w Elixirze wiąże się z dostępem do informacji o dacie i czasie systemu, co jest częstym zadaniem przy logowaniu, znakowaniu danych lub w dowolnej funkcji, która wymaga wiedzy o bieżącej dacie. Operacja ta jest niezbędna do tworzenia aplikacji świadomych czasu oraz do zadań takich jak generowanie raportów lub znaczników czasu w aplikacji internetowej.

## Jak to zrobić:
Standardowa biblioteka Elixira, poprzez moduł `DateTime`, umożliwia pobieranie bieżącej daty i czasu. Ponieważ Elixir działa na maszynie wirtualnej Erlanga (BEAM), wykorzystuje ona leżące u podstaw funkcjonalności Erlanga do operacji czasowych.

### Korzystając ze standardowej biblioteki Elixira
Elixir udostępnia funkcję `DateTime.utc_now/0`, aby uzyskać bieżącą datę i czas w UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Przykładowy wynik:**
```
~U[2024-02-05 19:58:40.925931Z]
```

Aby uzyskać tylko bieżącą datę, można wyodrębnić składniki roku, miesiąca i dnia:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Przykładowy wynik:**
```
~D[2023-05-04]
```

### Korzystając z biblioteki Timex
Dla bardziej złożonych wymagań dotyczących daty i czasu można wykorzystać popularną bibliotekę stron trzecich o nazwie Timex. Najpierw dodaj `Timex` do swoich zależności w pliku mix.exs:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Po zainstalowaniu zależności (`mix deps.get`), możesz użyć Timexa, aby uzyskać bieżącą datę:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Przykładowy wynik:**
```
~D[2023-05-04]
```

Timex oferuje obszerne funkcjonalności do manipulacji datą i czasem, co czyni go potężnym dodatkiem do twoich aplikacji Elixira, szczególnie przy pracy ze strefami czasowymi, formatowaniem i parsowaniem dat i czasów.

Rozumiejąc i wykorzystując wbudowane możliwości Elixira oraz bibliotekę Timex, możesz łatwo pracować z datami i czasami w swoich aplikacjach Elixira, dostosowując doświadczenie do potrzeb twojej aplikacji z precyzją i łatwością.
