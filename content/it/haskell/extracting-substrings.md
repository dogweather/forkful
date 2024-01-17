---
title:                "Estrazione di sottostringhe"
html_title:           "Haskell: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è l'estrazione di sottostringhe e perché i programmatori la fanno
L'estrazione di sottostringhe è il processo di ottenere una parte specifica di una stringa più grande. I programmatori spesso lo fanno per manipolare o analizzare i dati all'interno delle stringhe in modo più efficiente.

## Come fare:
Ecco un esempio di codice in Haskell per estrarre una sottostringa specifica da una stringa data:
```Haskell
-- Definizione di una funzione per estrarre una sottostringa
extractSubstring :: Int -> Int -> String -> String
extractSubstring start end str = take (end - start) (drop start str)

-- Esempio di utilizzo della funzione
main = do
    let str = "Questo è un esempio di stringa"
    let substr = extractSubstring 6 8 str -- Estrae la sottostringa "è un"
    putStrLn substr -- Stampa "è un"
```

## Approfondimento:
* Contesto storico: L'estrazione di sottostringhe è una tecnica utilizzata in molti linguaggi di programmazione, derivata dall'operazione di slicing nella programmazione a basso livello.
* Alternative: Esistono diverse funzioni predefinite in Haskell per estrarre sottostringhe, come `take` e `drop`. Inoltre, è possibile utilizzare espressioni regolari per estrarre parti di una stringa.
* Dettagli di implementazione: Nell'esempio sopra, la funzione `extractSubstring` prende tre parametri: la posizione di inizio, la posizione di fine e la stringa da cui estrarre. La funzione utilizza le funzioni `take` e `drop` per selezionare le parti desiderate della stringa.

## Vedi anche:
* Documentazione ufficiale di Haskell su come manipolare stringhe: https://www.haskell.org/tutorial/strings.html
* Un tutorial sulle espressioni regolari in Haskell: https://wiki.haskell.org/Regular_expressions
* Un esempio più avanzato di estrazione di sottostringhe utilizzando funzioni ricorsive: https://codereview.stackexchange.com/questions/84454/reversing-a-string-in-haskell