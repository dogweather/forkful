---
title:                "Fish Shell: Łączenie ciągów tekstowych"
simple_title:         "Łączenie ciągów tekstowych"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego
Wielu programistów często potrzebuje łączyć dwa lub więcej ciągów tekstu w jeden, w celu utworzenia nowego ciągu lub wyświetlenia danych w czytelny sposób. W tym artykule dowiesz się, jak wykorzystać polecenie concatenation w Fish Shell, aby skutecznie manipulować ciągami tekstowymi.

## Jak to zrobić
Wykonanie konkatenacji w Fish Shell jest bardzo proste - wystarczy użyć operatora plus (+) między dwoma ciągami tekstowymi. Przykładowy kod pokazujący ten proces wygląda następująco:

```Fish Shell
echo "Cześć" + " " + "świecie"
```
Wynik wyświetli się jako "Cześć świat" po użyciu polecenia echo. Możemy również przypisać wynik konkatenacji do zmiennej i wykorzystać ją później w kodzie.

```Fish Shell
set przywitanie "Cześć" + " " + "świecie"
echo $przywitanie
```
W tym przypadku zmienna "przywitanie" zawiera ciąg tekstowy "Cześć świat", który następnie jest wyświetlany przez polecenie echo.

## Deep Dive
Podczas konkatenacji ciągów tekstowych należy pamiętać o kilku ważnych kwestiach. Po pierwsze, polecenie concatenation można stosować tylko między dwoma ciągami tekstowymi - nie można łączyć ciągów z innymi typami danych, takimi jak liczby czy zmienne typu boolean.

Kolejną ważną rzeczą jest kolejność wykonywania polecenia. Jeśli będziemy używać operatora plus (+) między dwoma ciągami tekstowymi, to będzie również działać jako operator arytmetyczny i wykonać dodawanie. Aby uniknąć takiej sytuacji, należy użyć nawiasów, aby wyraźnie określić, że chcemy skonkatenować dwa ciągi, a nie wykonać dodawanie.

```Fish Shell
echo "5" + "2"  # Wynik: 7
echo ("5" + "2")  # Wynik: 52
```
W przypadku użycia innych operatorów niż plus (+), takich jak minus (-) lub gwiazdka (*), polecenie concatenation nie zadziała i otrzymamy błąd.

## Zobacz także
- Dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
- Przykłady zastosowania konkatenacji w Fish Shell: https://fishshell.com/docs/current/tutorial.html#combining
- Wideo tutorial o konkatenacji w Fish Shell: https://www.youtube.com/watch?v=JhKztLpl5yk