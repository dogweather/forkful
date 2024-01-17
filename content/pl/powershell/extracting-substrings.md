---
title:                "Ekstrakcja podciągów"
html_title:           "PowerShell: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wyodrębnianie podciągów to proces wyodrębniania pewnej części tekstu lub znaków z większego ciągu znaków. Programiści często wykorzystują to narzędzie w celu przetwarzania danych w celu uzyskania konkretnych informacji lub formatowania tekstu. Jest to wygodny sposób na manipulację i kontrolę nad danymi w programowaniu.

## Jak?

### Wyodrębnianie podciągów:

```PowerShell
$txt = "Witaj, to jest przykładowy tekst"
$txt.Substring(7, 16)
```
Ten kod wykorzystuje funkcję `Substring` do wyodrębnienia tekstu o długości 16 znaków rozpoczynając od 7 znaku, co daje nam wynik "to jest przykladowy".

### Formatowanie tekstu:

```PowerShell
$txt = "Tekst który chcemy <strong> wytłuszczyć </strong>"
$txt -replace "<[^>]*>", ""
```
W tym przykładzie wykorzystujemy funkcję `-replace` w celu usunięcia tagów HTML z tekstu. Wynik wyglądałby następująco: "Tekst który chcemy wytłuszczyć".

## Głębsza analiza

Wyodrębnianie podciągów jest powszechnie używane od wielu lat, szczególnie w językach programowania. W PowerShell istnieje wiele wbudowanych funkcji, takich jak `Substring`, `Split` i `Join`, które ułatwiają manipulację tekstów. Alternatywnie, programiści mogą również używać wyrażeń regularnych do wyodrębniania podciągów.

## Zobacz również

- [Dokumentacja Substring w PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_substrings)
- [Poradnik wyrażeń regularnych w PowerShell](https://regex101.com/r/vHXsQu/1)
- [Blog programistyczny PowerShell](https://powershellexplained.com/)