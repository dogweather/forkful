---
date: 2024-01-20 17:46:46.874122-07:00
description: "Wyci\u0105ganie podci\u0105g\xF3w to wydobywanie okre\u015Blonych cz\u0119\
  \u015Bci z wi\u0119kszego ci\u0105gu tekstowego. Programi\u015Bci robi\u0105 to,\
  \ by manipulowa\u0107 danymi, weryfikowa\u0107 formaty, czy\u2026"
lastmod: '2024-03-13T22:44:35.616758-06:00'
model: gpt-4-1106-preview
summary: "Wyci\u0105ganie podci\u0105g\xF3w to wydobywanie okre\u015Blonych cz\u0119\
  \u015Bci z wi\u0119kszego ci\u0105gu tekstowego."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

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
