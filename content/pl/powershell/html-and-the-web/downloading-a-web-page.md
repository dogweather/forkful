---
title:                "Pobieranie strony internetowej"
aliases: - /pl/powershell/downloading-a-web-page.md
date:                  2024-01-20T17:44:41.242455-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
"Co to jest i po co?"
Pobieranie strony internetowej to zapisanie jej zawartości na dysk. Programiści robią to, aby przetworzyć dane, pobrać ważne informacje lub zarchiwizować zawartość.

## How to:
"Jak to zrobić:"
Uruchom poniższy skrypt, aby pobrać zawartość strony:

```PowerShell
Invoke-WebRequest -Uri "http://przykladowa-strona.pl" -OutFile "strona.html"
```

Wynik to zapisany plik `strona.html` z zawartością strony.

Aby czytać zawartość jako tekst bezpośrednio w PowerShellu:

```PowerShell
$response = Invoke-WebRequest -Uri "http://przykladowa-strona.pl"
$response.Content
```

Otrzymany wynik będzie surowym HTMLem strony.

## Deep Dive:
"Wnikliwe spojrzenie:"
Pobieranie stron sieciowych istnieje od początków internetu. Alternatywą dla `Invoke-WebRequest` jest użycie `curl` lub `wget` w systemie Unix. `Invoke-WebRequest` jest mocno zintegrowane z .NET Framework, co pozwala na łatwe przetwarzanie danych w PowerShellu. Korzysta z klasy `HttpClient` w tle.

## See Also:
"Zobacz również:"
- [Dokumentacja PowerShell Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest)
- [Learn PowerShell](https://learn.microsoft.com/pl-pl/powershell/) - strona nauki PowerShell po polsku.
