---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:16:08.482949-07:00
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego? Pobieranie aktualnej daty pozwala aplikacjom na dostosowanie się do czasu rzeczywistego. Programiści używają tej funkcji do logowania, timestampów i funkcjonalności zależnych od czasu.

## How to:
Poniżej znajdziesz przykłady kodu PHP pokazujące, jak uzyskać aktualną datę:

```PHP
<?php
echo date("Y-m-d"); // Wyświetla aktualną datę w formacie RRRR-MM-DD
?>
```
Sample output:
```
2023-04-01
```

Aby uzyskać datę i czas:

```PHP
<?php
echo date("Y-m-d H:i:s"); // Wyświetla datę i czas
?>
```
Sample output:
```
2023-04-01 15:20:30
```
Używamy funkcji `date()`, która przyjmuje format daty jako parametr.

## Deep Dive
Trochę historii, alternatyw i szczegółów implementacyjnych dotyczących uzyskiwania aktualnej daty. Funkcja `date()` istnieje w PHP od jego pierwszych wersji. Jest prosta w użyciu, ale musisz pamiętać o ustawieniu odpowiedniej strefy czasowej w konfiguracji PHP (`php.ini`) lub w samym skrypcie przy pomocy funkcji `date_default_timezone_set()`.

Jeśli potrzebujesz bardziej obiektowego podejścia, możesz użyć klasy `DateTime`:

```PHP
<?php
$datetime = new DateTime();
echo $datetime->format('Y-m-d H:i:s');
?>
```

Istnieją też alternatywne klasy jak `DateTimeImmutable` lub funkcje jak `getdate()`, które też pozwalają na manipulację i odczyt daty oraz czasu.

## See Also
Sprawdź też inne funkcje i klasy w dokumentacji PHP:

- Dokumentacja `date()`: https://www.php.net/manual/en/function.date.php
- Dokumentacja `DateTime` klasy: https://www.php.net/manual/en/class.datetime.php
- Jak ustawić strefę czasową w PHP: https://www.php.net/manual/en/function.date-default-timezone-set.php
- Alternatywna funkcja `getdate()`: https://www.php.net/manual/en/function.getdate.php
