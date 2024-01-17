---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "PowerShell: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków pasujących do wzorca to proces, w którym programiści szukają i usuwają określone znaki w tekście lub ciągu znaków. Robią to, aby uprościć i uporządkować dane lub przygotować je do dalszych działań.

## Jak to zrobić:
```PowerShell
# Przykładowe dane w postaci ciągu znaków
$string = "To jest *przykładowy* tekst z *znakami* specjalnymi."

# Usuwanie znaków specjalnych "*"
$string -replace "\*", ""
```

```PowerShell
# Przykładowe dane w postaci tablicy
$array = "zielona", "czerwona", "niebieska", "*żółta*"

# Usuwanie znaków specjalnych "*"
$array -replace "\*", ""

# Wynik:
zielona
czerwona
niebieska
żółta
```

## Głębsze zagadnienia:
1. Kontekst historyczny: Technika usuwania znaków pasujących do wzorca została zapoczątkowana przez język programowania Perl w latach 80. XX wieku. Od tego czasu stała się niezwykle popularna w wielu innych językach, w tym w PowerShell.
2. Alternatywy: Istnieją również inne sposoby na usuwanie znaków pasujących do wzorca, takie jak funkcje wbudowane w języki programowania lub zewnętrzne biblioteki.
3. Szczegóły implementacji: W PowerShell, metoda "-replace" jest wykorzystywana do usuwania znaków pasujących do wzorca. Najpierw należy podać wzorzec (w postaci wyrażenia regularnego), a następnie znak lub ciąg znaków, które mają zostać usunięte.

## Zobacz również:
- Dokumentacja Microsoft dotycząca funkcji "-replace": https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1#using-the-replace-method-to-remove-characters
- Wstęp do wyrażeń regularnych w języku PowerShell: https://dzone.com/articles/regular-expressions-in-windows-power-shell