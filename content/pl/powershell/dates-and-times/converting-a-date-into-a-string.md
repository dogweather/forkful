---
title:                "Konwersja daty na łańcuch znaków"
aliases:
- /pl/powershell/converting-a-date-into-a-string/
date:                  2024-01-20T17:37:29.994754-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Co to jest konwersja daty na łańcuch znaków? To proces zmiany formatu daty z jej wbudowanego typu w PowerShell do postaci tekstowej, czyli stringa. Po co programiści to robią? Aby ułatwić wyświetlanie, zapisywanie lub manipulowanie datami w interfejsach użytkownika, plikach, lub systemach nieobsługujących typów dat.

## How to:

Załóżmy, że masz datę i chcesz ją przekształcić na tekst. W PowerShell robisz to tak:

```PowerShell
# Aktualna data i czas
$teraz = Get-Date

# Konwersja na string w domyślnym formacie
$stringDaty = $teraz.ToString()

# Wyświetlanie wyniku
$stringDaty
```

Możesz również określić własny format wyjściowy:

```PowerShell
# Formatowanie daty w określonym formacie np. YYYY-MM-DD
$dostosowanyStringDaty = $teraz.ToString('yyyy-MM-dd')

# Wyświetlanie wyniku
$dostosowanyStringDaty
```

Wynikowy string będzie wyglądał tak:

```
2023-04-05
```

To tylko dwa proste przykłady. Możliwości formatowania są o wiele szersze.

## Deep Dive:

Konwersja daty na string jest niezbędna, gdyż format daty różni się w zależności od kontekstu użycia. Na przykład, logi systemowe mogą wymagać jednego formatu, a raporty dla użytkownika końcowego innego.

Historia formatowania dat sięga jeszcze języków programowania, które były używane przed PowerShell, takich jak C czy Perl, gdzie również istniała potrzeba prezentacji dat w czytelnej formie.

Alternatywy do `.ToString()` to używanie metod formatujących jak `-f`, czy cmdletów jak `Get-Date` z parametrem `-Format`:

```PowerShell
# Użycie -f do formatowania
$formattedDate = '{0:yyyy-MM-dd}' -f (Get-Date)

# Użycie cmdleta Get-Date z parametrem -Format
$cmdletFormattedDate = Get-Date -Format 'yyyy-MM-dd'
```

Podczas implementacji warto zwrócić uwagę na kulturotwórcze ustawienia formatowania daty (`CultureInfo`), ponieważ to one decydują o domyślnym formacie daty i czasu.

## See Also:

Oto kilka przydatnych linków, jeśli chcesz zgłębić temat:

- Dokumentacja Microsoft o cmdlecie `Get-Date`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date
- Informacje o klasie `DateTime` w .NET, której używa PowerShell: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Strona o formatowaniu niestandardowym ciągów daty i czasu: https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
