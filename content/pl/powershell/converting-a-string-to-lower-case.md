---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "PowerShell: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwertowanie ciągów znaków na małe litery jest ważnym aspektem programowania, który pozwala na jednolite formatowanie tekstu. Jest to proces zmiany wszystkich znaków w wybranym ciągu na ich małe odpowiedniki. Programiści często wykorzystują tę funkcjonalność do porównywania i sortowania danych, a także do wyświetlania tekstu w czytelny sposób.

## Jak to zrobić:
Aby skonwertować ciąg znaków na małe litery w języku PowerShell, wystarczy użyć wbudowanej funkcji ```ToLower()```. Przykładowo, chcąc skonwertować ciąg "HELLO WORLD" na małe litery, należy wykonać następującą komendę:

```PowerShell
"HELLO WORLD".ToLower()
```

## Wnikliwa analiza:
Konwertowanie ciągów znaków na małe litery jest możliwe dzięki funkcji ```.ToLower()```, która jest dostępna w większości języków programowania. Pomysł na implementację tej funkcji powstał w latach pięćdziesiątych, kiedy to programiści potrzebowali sposobu na porównywanie tekstów bez względu na wielkość liter. Alternatywnym sposobem na osiągnięcie tego efektu jest użycie funkcji ```.ToUpper()```, która konwertuje znaki na duże litery.

## Zobacz również:
Jeśli chcesz dowiedzieć się więcej o funkcjonowaniu funkcji ```.ToLower()``` w języku PowerShell, zapoznaj się z oficjalną dokumentacją na stronie [Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/string-methods/tolower?view=powershell-7.1).