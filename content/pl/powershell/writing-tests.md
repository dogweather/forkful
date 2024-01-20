---
title:                "Pisanie testów"
html_title:           "PowerShell: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Czym & Po co?

Pisanie testów jest częstym aspektem pracy programisty. Polega ono na tworzeniu kodu, który będzie sprawdzał poprawność działania innych fragmentów naszego kodu. Jest to ważne, ponieważ pomaga w szybszym wykrywaniu błędów i ułatwia odnalezienie ich przyczyn.

## Jak to zrobić?

Sprawdzenie działania naszego kodu może być wykonane w bardzo prosty sposób, poprzez użycie komendy `test`. Możemy również skorzystać z modułu `Pester`, który oferuje bardziej rozbudowaną funkcjonalność testowania.

```PowerShell
test { 2 + 2 -eq 4 }
Pester { 2 + 2 -eq 4 }
```

Oczekiwany wynik dla obu przykładów jest taki sam - `True`.

## Głębsze zagadnienia

Pisanie testów jest praktykowane od dłuższego czasu, jednak dopiero niedawno zyskało na popularności. Alternatywą dla Pester jest framework `PSDeploy`, który umożliwia testowanie poprawności skryptów deploymentowych.

Jeśli chcemy napisać bardziej skomplikowane testy, możemy skorzystać z zaawansowanej składni, która pozwala na tworzenie różnych asercji i ustawianie warunków testowych.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o testowaniu w PowerShell, polecamy następujące źródła:

- [Książka "PowerShell in Depth"](https://www.manning.com/books/powershell-in-depth)