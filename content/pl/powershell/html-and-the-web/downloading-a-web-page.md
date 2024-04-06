---
date: 2024-01-20 17:44:41.242455-07:00
description: "How to: \"Jak to zrobi\u0107:\" Uruchom poni\u017Cszy skrypt, aby pobra\u0107\
  \ zawarto\u015B\u0107 strony."
lastmod: '2024-04-05T21:53:37.053728-06:00'
model: gpt-4-1106-preview
summary: "\"Jak to zrobi\u0107:\" Uruchom poni\u017Cszy skrypt, aby pobra\u0107 zawarto\u015B\
  \u0107 strony."
title: Pobieranie strony internetowej
weight: 42
---

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
