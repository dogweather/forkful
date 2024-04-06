---
date: 2024-01-20 17:46:46.874122-07:00
description: "Jak to zrobi\u0107: Historia m\xF3wi, \u017Ce wyci\u0105ganie podci\u0105\
  g\xF3w istnia\u0142o od samego pocz\u0105tku programowania jako metoda obr\xF3bki\
  \ tekst\xF3w. W PowerShell, kt\xF3ry pojawi\u0142\u2026"
lastmod: '2024-04-05T21:53:37.041987-06:00'
model: gpt-4-1106-preview
summary: "Historia m\xF3wi, \u017Ce wyci\u0105ganie podci\u0105g\xF3w istnia\u0142\
  o od samego pocz\u0105tku programowania jako metoda obr\xF3bki tekst\xF3w."
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
