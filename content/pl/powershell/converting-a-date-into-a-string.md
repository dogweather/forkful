---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja daty na łańcuch znaków to proces przekształcenia obiektu daty(lub czasu) do formatu tekstowego, znalazł zastosowanie dlatego, że jest bardziej elastyczny dla użytkowników i łatwiejszy w dowolnym manipulowaniu.

## Jak to zrobić:
Aby przekonwertować datę na łańcuch znaków w PowerShell, możemy użyć polecenia `Get-Date` z parametrem `-Format`. Oto przykładowy kod:

```PowerShell
# Utwórz aktualną datę i czas
$teraz = Get-Date
# Wyświetl oryginalną wartość
$teraz

# Konwertuj do formatu "rok-miesiąc-dzień"
$dataFormat = $teraz.ToString("yyyy-MM-dd")
# Wyświetl sformatowaną wartość
$dataFormat
```
Wykonanie powyższego kodu włącznie z wyjściem powinno wyglądać tak:

```PowerShell
# Wartość oryginalna
Czwartek, 29 lipca 2021 12:24:56

# Sformatowana wartość
2021-07-29
```
## Dogłębne zrozumienie
Początki konwersji daty na łańcuch znaków sięgają początków programowania, kiedy manipulowanie danymi było trudniejsze. Teraz, będąc standardowym elementem większości języków programowania, służy do ułatwienia użytkownikom odczytywania dat, sortowania elementów, generowania raportów itp.

Mimo, że w PowerShell korzystamy z `Get-Date` z `-Format`, dostępne są także inne metody, jak `ToShortDateString()` czy `ToLongDateString()`, które poprawiają czytelność daty.

Szczegół implementacji polega na tym, że konwersja wykorzystuje klasy `.NET`, które zapewniają szerokie możliwości manipulacji i konwersji dat. PowerShell, jako język oparty na platformie .NET, dziedziczy te możliwości.

## Zobacz również
Zobacz te zasoby, jeśli chcesz dowiedzieć się więcej o pracy z datami w PowerShell:
2. [Formatowanie dat i czasu w .NET](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/standard-date-and-time-format-strings)
3. [Get-Date w PowerShell](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)