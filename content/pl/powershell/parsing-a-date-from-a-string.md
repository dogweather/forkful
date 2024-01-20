---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty z ciągu znaków to proces wyodrębniania określonej informacji o dacie (np. dni, miesiące, lata) z ciągu tekstowego. Programiści robią to, aby móc operować na danych datowych i wykorzystywać je w różnych funkcjach programu.

## Jak to zrobić:

```PowerShell
# przykładowy ciąg znaków
$dataStr = '14-02-2021'

# parsowanie daty
$parsowanaData = [datetime]::ParseExact($dataStr, 'dd-MM-yyyy', $null)

# wydrukowanie parsowanej daty
$parsowanaData
```
Przykładowe wyniki:
```PowerShell
February 14, 2021 00:00:00
```

## Pogłębione informacje:

(1) W kontekście historycznym - pomysł parsowania daty z ciągu znaków pojawił się wraz z narodzinami języków programowania. W primerzych językach programowania napisane były dedykowane funkcje do tego celu. W PowerShell (używając .NET Framework) operacja parsowania daty stała się znacznie prostsza i bardziej elastyczna. 

(2) Alternatywy - warto wiedzieć, że istnieje wiele różnych formatów czasu i zależnie od regionu, parsowanie daty może wymagać innej sekwencji znaków. Istnieje także możliwość uzycia `TryParseExact()`, która zamiast zgłaszać wyjątek podczas błędnego parsowania, zwróci wartość logiczną `False`.

(3) Szczegóły implementacji - podczas parsowania daty, argumenty funkcji `ParseExact()` to: (1) ciąg, który ma zostać przeparsowany, (2) format ciągu i (3) dostawca formatu (który może być `null`, jeżeli nie jest wymagany konkretny format). 

## Zobacz również:

- Dokumentacja Microsoft na temat metody ParseExact: [https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parseexact?view=net-5.0](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parseexact?view=net-5.0)
- Dokumentacja PowerShell na temat operacji z datą: [https://docs.microsoft.com/pl-pl/powershell/scripting/samples/working-with-dates-and-times?view=powershell-7.1](https://docs.microsoft.com/pl-pl/powershell/scripting/samples/working-with-dates-and-times?view=powershell-7.1)