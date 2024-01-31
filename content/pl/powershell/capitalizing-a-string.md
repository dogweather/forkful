---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means changing the first letter of each word to uppercase. Programmers do this to format text data consistently or for aesthetic reasons in user interfaces.

## How to:
PowerShell ma wbudowany sposób na zamianę stringów na formę z wielkimi literami. Użyj `ToTitleCase` z `TextInfo`:

```PowerShell
$text = "oto przykład tekstu"
$culture = [System.Globalization.CultureInfo]::CurrentCulture
$textInfo = $culture.TextInfo
$titleCasedText = $textInfo.ToTitleCase($text)
$titleCasedText
```
Output:
```
Oto Przykład Tekstu
```

Możesz też użyć metody `.ToUpper()` i `.ToLower()` do manipulacji całością:

```PowerShell
$lowerText = "oto inny przykład"
$upperText = $lowerText.ToUpper()
$upperText
```
Output:
```
OTO INNY PRZYKŁAD
```

## Deep Dive
Capitalizing strings nie jest nowością. W językach programowania takich jak C# czy JavaScript, operacje na capitalization są standardem. PowerShell korzysta z .NET, więc możemy używać tych samych metod.

Inną opcją jest manualne przejście przez string i zmiana liter. Jest to bardziej skomplikowane i mniej wydajne, ale pokazuje, jak działa proces:

```PowerShell
function Convert-ToTitleCase($inputString) {
    $words = $inputString -split ' '
    $titleCasedWords = $words | ForEach-Object { $_.Substring(0,1).ToUpper() + $_.Substring(1).ToLower() }
    return $titleCasedWords -join ' '
}

Convert-ToTitleCase "kolejny przykład do pokazania"
```

Alternatywnie, istnieją gotowe narzędzia i biblioteki pomocnicze, takie jak Humanizer, które można zintegrować z PowerShell, aby ułatwić różne manipulacje stringami.

Gdy mowa o implementacji, ważne jest, aby pamiętać o różnych konwencjach pisowni związanych z językiem, kulturą czy specjalnymi przypadkami, jak skróty czy akronimy.

## See Also
- Microsoft Documentation on `ToTitleCase`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)
- Humanizer Library: [Link](https://github.com/Humanizr/Humanizer)
