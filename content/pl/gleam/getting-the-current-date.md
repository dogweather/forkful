---
title:    "Gleam: Otrzymywanie bieżącej daty"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czas jest istotnym elementem w wielu projektach programistycznych. Możliwość uzyskania aktualnej daty może być kluczowa w budowaniu aplikacji, które wykorzystują informacje czasowe. W tym artykule omówimy, jak w prosty sposób uzyskać aktualną datę w języku programowania Gleam.

## Jak to zrobić

Aby uzyskać aktualną datę w Gleam, musimy użyć standardowej biblioteki czasu `gleam/time`. Wpierw należy zaimportować tę bibliotekę, a następnie wywołać funkcję `now()` z modułu `Time`:

```
import gleam/time

my_date = Time.now()
```

Funkcja `now()` zwraca wartość typu `Date` zawierającą aktualną datę i czas. Możemy wyświetlić tę wartość używając funkcji `to_iso8601()` w celu uzyskania daty w formacie ISO-8601:

```
import gleam/time

my_date = Time.now()
my_date_str = Time.Date.to_iso8601(my_date)

IO.println("Aktualna data: #{my_date_str}")
```

Po uruchomieniu tego kodu, powinniśmy zobaczyć aktualną datę w formacie ISO-8601 w konsoli:

```
Aktualna data: 2021-04-28T09:00:00.123Z
```

## Głębsze zrozumienie

W języku Gleam, daty są reprezentowane za pomocą typu `Date` zawierającego pola `year`, `month`, `day`, `hour`, `minute`, `second` oraz `nanosecond`. Dzięki temu, możemy z łatwością wyodrębnić poszczególne składniki daty i wyświetlić je w wybranym formacie.

W większości przypadków, funkcja `now()` zwróci aktualną datę w strefie czasowej reprezentowanej przez system operacyjny. Jednak, jeśli potrzebujemy daty w określonej strefie czasowej, możemy użyć funkcji `convert()` z modułu `Time.Zone` aby przekonwertować datę do odpowiedniego formatu.

## Zobacz także

- Dokumentacja Gleam dotycząca biblioteki czasu: <link>https://gleam.run/documentation/standard-libraries/time/</link>
- Dalsze informacje o formatach daty i czasu w Gleam: <link>https://gleam.run/documentation/guides/dates-and-time/</link>
- Przykładowa aplikacja Gleam wykorzystująca aktualną datę: <link>https://github.com/gleam-lang/example-date-app</link>