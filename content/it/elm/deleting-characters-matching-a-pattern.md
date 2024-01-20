---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Eliminare caratteri che corrispondono a un certo modello è un processo di nettezza del codice e di manipolazione delle stringhe. I programmatori lo fanno per pulire i dati di input, manipolare i testi e affinare le ricerche.

## Come fare:

Di seguito, un esempio di come eliminare caratteri che corrispondono a un modello in Elm. 

```Elm
import String

rimuoviCaratteri : String -> String
rimuoviCaratteri str = 
    String.foldl 
        (\c acc -> if String.contains (String.fromChar c) "#$%" then acc else acc ++ (String.fromChar c))
        ""
        str

main =
    print (rimuoviCaratteri "Ciao##$$%%Mondo")
```
Risultato di compilazione:
`"CiaoMondo"`

Nell’esempio sopra, la funzione `rimuoviCaratteri` rimuove tutti i caratteri "#", "$", "%" dalla stringa "Ciao##$$%%Mondo".

## Approfondimento 

L'eliminazione dei caratteri corrispondenti a un modello è un concetto di programmazione radicato storico, dai giorni di PERL e delle espressioni regolari. In Elm, abbiamo usato `String.foldl` e `String.contains` al posto di espressioni regolari. 

In alternativa, potreste usare il pacchetto elm/regex per un approccio più simile alle espressioni regolari. 

Dettagli di implementazione: `String.foldl` scorre la stringa dall'inizio alla fine, controllando e costruendo una nuova stringa senza i caratteri indesiderati. `String.contains` viene usato per verificare se un carattere fa parte del modello.

## Vedi anche:

- Documentazione di Elm String [Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Documentazione di Elm foldl [Elm foldl](https://package.elm-lang.org/packages/elm/core/latest/List#foldl)
- Guida alla programmazione Elm [Elm Tutorial](https://elmprogramming.com/) 
- Pacchetto Elm Regex [Elm Regex](https://package.elm-lang.org/packages/elm/regex/latest/)
- Guida alle espressioni regolari [Regex Guide](https://www.regular-expressions.info/)