---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:39:09.168946-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsing daty z ciągu tekstowego to przekształcanie tekstu na typ daty. Programiści robią to, aby obsłużyć daty w różnych formatach i łatwo manipulować czasem w aplikacjach.

## How to (Jak to zrobić):
```PowerShell
# Prosty przykład parsowania daty
$dateString = "2023-04-01"
$date = [datetime]::Parse($dateString)
Write-Output $date

# Format niestandardowy
$customDateString = "01 kwietnia 2023"
$customFormat = "dd MMMM yyyy"
$culture = [Globalization.CultureInfo]::CreateSpecificCulture("pl-PL")
$customDate = [datetime]::ParseExact($customDateString, $customFormat, $culture)
Write-Output $customDate
```

Przykładowe wyjście:
```
Saturday, April 1, 2023 12:00:00 AM
Friday, April 1, 2023 12:00:00 AM
```

## Deep Dive (Głębsze zagadnienia):
Parsing daty sięga czasów, gdy pierwsze aplikacje zaczęły wymagać obsługi różnych formatów daty. W PowerShellu prosty parsing dokonuje się z wykorzystaniem wbudowanej metody `[datetime]::Parse()`. Kiedy format daty jest nietypowy lub różni się w zależności od ustawień regionalnych, używamy metody `[datetime]::ParseExact()` razem z obiektem `CultureInfo`, aby precyzyjnie określić, jakich reguł należy użyć.

Alternatywą jest używanie cmdletu `Get-Date`, który również umożliwia konwersję ciągów tekstowych na daty, z opcją `-Format` do określenia formatu zdjęciowego.

Ciekawostką jest fakt, że systemy operacyjne i aplikacje często posiadają wbudowane normy określające format daty zgodnie z ustaleniami regionalnymi, co z kolei wpływa na interpretację i walidację dat.

## See Also (Zobacz również):
- Dokumentacja Microsoftu na temat `[datetime]` w PowerShellu: [Microsoft Docs - datetime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-7.0)
- Informacje o `[CultureInfo]` i obsłudze różnych formatów kulturowych: [Microsoft Docs - CultureInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-7.0)
- Opis cmdletu `Get-Date` w PowerShellu: [Microsoft Docs - Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
