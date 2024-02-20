---
date: 2024-01-20 17:44:41.242455-07:00
description: "\"Co to jest i po co?\" Pobieranie strony internetowej to zapisanie\
  \ jej zawarto\u015Bci na dysk. Programi\u015Bci robi\u0105 to, aby przetworzy\u0107\
  \ dane, pobra\u0107 wa\u017Cne\u2026"
lastmod: 2024-02-19 22:04:54.769952
model: gpt-4-1106-preview
summary: "\"Co to jest i po co?\" Pobieranie strony internetowej to zapisanie jej\
  \ zawarto\u015Bci na dysk. Programi\u015Bci robi\u0105 to, aby przetworzy\u0107\
  \ dane, pobra\u0107 wa\u017Cne\u2026"
title: Pobieranie strony internetowej
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
