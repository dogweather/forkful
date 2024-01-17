---
title:                "Używanie wyrażeń regularnych"
html_title:           "PowerShell: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

Wykorzystywanie wyrażeń regularnych jest jednym z najważniejszych narzędzi używanych przez programistów. Pozwalają one na szybkie i precyzyjne wyszukiwanie oraz manipulację tekstem. Jest to szczególnie przydatne przy przetwarzaniu danych, sprawdzaniu poprawności wprowadzanych informacji oraz w tworzeniu złożonych skryptów.

## Jak to zrobić?

Używanie wyrażeń regularnych jest bardzo proste w języku PowerShell. Musisz stworzyć wyrażenie i przekazać je do funkcji odpowiedzialnej za przeszukiwanie tekstu. Poniżej znajdziesz przykłady kodów oraz ich wyników.

### Wyszukiwanie słowa "programowanie" w tekście
```PowerShell
$text = "Cześć! Lubię programowanie w PowerShell!"
$text | Select-String -Pattern "programowanie"

Wynik:
Lubię programowanie w PowerShell!
```

### Zastępowanie wyrazu "świat" w tekście
```PowerShell
$text = "Witaj, świecie!"
$text -replace "świat", "środowisko"

Wynik:
Witaj, środowisko!
```

### Sprawdzanie czy dana wartość jest liczbą
```PowerShell
$value = "100"
$value -match "^\d+$"

Wynik:
True
```

## Głębsze spojrzenie

Wyrażenia regularne są powszechnie stosowane od ponad 60 lat. Powstały na potrzeby edytorów tekstu i zostały później zaimplementowane w wielu językach programowania, w tym również w PowerShell. Alternatywnym sposobem na przetwarzanie tekstu jest użycie poleceń warunkowych i pętli, co jednak może być czasochłonne i mniej precyzyjne. W PowerShell, wyrażenia regularne są implementowane przez specjalne obiekty typu [Regex](), co pozwala na użycie różnych metod i właściwości, które ułatwiają pracę z nimi. Więcej informacji na temat wyrażeń regularnych w PowerShell można znaleźć w [dokumentacji Microsoft](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7).

## Zobacz także

- [Regex101](https://regex101.com/) - narzędzie online do testowania i eksperymentowania z wyrażeniami regularnymi.
- [PowerShell dla programistów](https://github.com/vexx32/PowerShell-Docs/blob/pre-release/languages/pl-PL/PSLogic.md) - przydatny poradnik zawierający wiele przykładów i wyjaśnień dotyczących programowania w PowerShell.
- [Python dla każdego](https://www.python.pl/) - serwis z artykułami i poradnikami dla początkujących programistów, w którym znajdziesz również wiele informacji na temat wyrażeń regularnych w języku Python.