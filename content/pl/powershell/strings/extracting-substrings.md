---
title:                "Wycinanie podłańcuchów"
aliases: - /pl/powershell/extracting-substrings.md
date:                  2024-01-20T17:46:46.874122-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyciąganie podciągów to wydobywanie określonych części z większego ciągu tekstowego. Programiści robią to, by manipulować danymi, weryfikować formaty, czy też analizować i przetwarzać informacje tekstowe.

## Jak to zrobić:

```PowerShell
# Wyciąganie podciągu od konkretnej pozycji
$tekst = "Dzień dobry, świecie!"
$podciag = $tekst.Substring(7, 5)
$podciag  # wyświetli 'dobry'

# Użycie metody .Split() do wycięcia słowa
$czesci = $tekst.Split(' ')
$czesci[1]  # wyświetli 'dobry'

# Wycinanie od początku do znaku
$index = $tekst.IndexOf('ś')
$poczatekDoZnaku = $tekst.Substring(0, $index)
$poczatekDoZnaku  # wyświetli 'Dzień dobry, '

# Wycinanie od znaku do końca
$znakDoKonca = $tekst.Substring($index)
$znakDoKonca  # wyświetli 'świecie!'
```

## Zanurz się głębiej:

Historia mówi, że wyciąganie podciągów istniało od samego początku programowania jako metoda obróbki tekstów. W PowerShell, który pojawił się na scenie w 2006 roku, operacje na ciągach są wszechstronne i łatwe dzięki metodzie `.Substring()`, operatorom takim jak `-split` oraz potężnemu pipeliningowi.

Alternatywy? Można używać wyrażeń regularnych (regex) dla bardziej złożonych zadań, operatora `-match`, czy wpierającej .NET klasy `[string]`.

Jeśli chodzi o szczegóły implementacyjne, w PowerShell `.Substring()` pochodzi bezpośrednio z .NET i jest równie wydajny. Pamiętaj, że pozycje są liczone od zera.

## Zobacz też:

- Poradnik do `Split` i `-split` operator: [ss64.com/ps/split.html](https://ss64.com/ps/split.html)
