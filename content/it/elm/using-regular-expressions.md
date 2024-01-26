---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari sono strumenti per trovar corrispondenze in stringhe di testo. Le usiamo perché sono efficienti per la validazione, la ricerca e la manipolazione di testo.

## How to:
Elm usa il pacchetto `regex` per gestire le espressioni regolari. Ecco come usarlo:

```elm
import Regex exposing (fromString, contains)

main =
    let
        regex = fromString "^[a-zA-Z0-9]*$"
        isMatch = contains regex
    in
    isMatch "ContenutoValido123" -- True
```

Esempio con sostituzione:

```elm
import Regex exposing (replace, All, regex)

main =
    let
        pattern = regex "\\s+"
        replacement = "_"
        cleanString = replace All pattern (\_ -> replacement)
    in
    cleanString "Sostituisci   spazi   con underscore." -- "Sostituisci_spazi_con_underscore."
```

## Deep Dive
Elm richiede una virgola di escape aggiuntiva quando si usano espressioni regolari. Prima dell'avvento dei pacchetti dedicati le alternative erano limitate, ma ora il pacchetto `regex` è robusto e ben integrato. Funziona convertendo le espressioni regolari in motori di ricerca interni ottimizzati per Elm.

## See Also
- Documentazione Elm [`Regex` package](https://package.elm-lang.org/packages/elm/regex/latest/).
- [elm-community/string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/): offre funzioni di manipolazione delle stringhe che possono essere usate insieme alle espressioni regolari.
- Tutorial su espressioni regolari: [RegexOne](https://regexone.com/).
