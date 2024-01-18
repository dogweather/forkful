---
title:                "Analiza daty z ciągu znaków."
html_title:           "PowerShell: Analiza daty z ciągu znaków."
simple_title:         "Analiza daty z ciągu znaków."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Parsing daty z ciągu znaków to po prostu sposobem na dostarczenie programowi dokładnej informacji o dacie, zamiast pozostawiania jej w formie niejasnego tekstu. Programiści korzystają z parsingu daty, aby dokonać bezpiecznego i dokładnego przetwarzania dat w swoich programach.

## Jak to zrobić:
~~~~
PowerShell
# Przykłady użycia funkcji Get-Date do parsowania daty z ciągu znaków
Get-Date "15-09-2020"
# Wynik: 15 września 2020, 00:00:00

Get-Date "09/15/2020"
# Wynik: 15 września 2020, 00:00:00

Get-Date "Sep 15, 2020"
# Wynik: 15 września 2020, 00:00:00

# Można również sprecyzować format daty w celu dokładniejszego parsowania
Get-Date "15 września 2020" -Format "dd MMMM yyyy"
# Wynik: 15 września 2020, 00:00:00

~~~~

## Wprowadzenie w głąb:
Pierwsze narzędzia do parsowania daty pojawiły się już w latach 70. XX wieku, ale dopiero w latach 90. zaczęto wprowadzać bardziej elastyczne metody parsowania daty. Alternatywą dla użycia funkcji Get-Date jest korzystanie z biblioteki .NET Framework DateTime.ParseExact(), która umożliwia bardziej szczegółowe specyfikowanie formatu daty. Z implementacyjnego punktu widzenia, parsowanie daty polega na przetworzeniu ciągu znaków na obiekt reprezentujący datę i czas.

## Zobacz również:
- Dokumentacja funkcji Get-Date: https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7
- Przykłady użycia funkcji Get-Date: https://adamtheautomator.com/get-date-powershell/
- Dokumentacja metody DateTime.ParseExact(): https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parseexact?view=netcore-3.1