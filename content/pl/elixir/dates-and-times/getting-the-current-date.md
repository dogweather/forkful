---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:30.203696-07:00
description: "Pobieranie bie\u017C\u0105cej daty w Elixirze wi\u0105\u017Ce si\u0119\
  \ z dost\u0119pem do informacji o dacie i czasie systemu, co jest cz\u0119stym zadaniem\
  \ przy logowaniu, znakowaniu\u2026"
lastmod: 2024-02-19 22:04:54.232545
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w Elixirze wi\u0105\u017Ce si\u0119 z\
  \ dost\u0119pem do informacji o dacie i czasie systemu, co jest cz\u0119stym zadaniem\
  \ przy logowaniu, znakowaniu\u2026"
title: Pobieranie aktualnej daty
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
