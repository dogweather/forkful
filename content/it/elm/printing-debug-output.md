---
title:    "Elm: Stampa di output di debug"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare i messaggi di debug è un'importante pratica di programmazione che può aiutare a identificare ed eliminare errori nel tuo codice. Invece di provare a capire dove il tuo programma sta fallendo, la stampa dei messaggi di debug ti permette di vedere esattamente ciò che sta accadendo durante l'esecuzione del codice. Questo può risparmiare tempo e frustrazione nella risoluzione di bug.

## Come fare

Per stampare i messaggi di debug in Elm, è possibile utilizzare la funzione `Debug.log`, che accetta due argomenti: una stringa con il messaggio di debug e un valore da stampare.

```Elm
Debug.log "Debug message" 10
```

Questo codice produrrà il seguente output:

```
Debug message= 10
```

Puoi anche stampare valori più complessi, come liste o record, utilizzando la funzione `toString` per convertirli in stringhe.

```Elm
import Debug

record = {name = "Mario", age = 30}

Debug.log "Record" (toString record)
```

Questo produrrà l'output:

```
Record = "{ name = \"Mario\", age = 30 }"
```

## Approfondimento

Mentre la stampa dei messaggi di debug può essere utile per identificare errori nel tuo codice, è importante non abusare di questa pratica. Troppi messaggi di debug possono essere confusi e rendere il codice meno leggibile. Inoltre, ricorda di rimuovere i messaggi di debug una volta risolti i bug.

Inoltre, è possibile utilizzare il parametro `Always` nella funzione `Debug.log` per stampare i messaggi di debug anche in produzione. Questo può essere utile per verificare che il tuo codice sta funzionando correttamente in ogni contesto.

## Vedi anche

- [Documentazione di Elm sul debug](https://guide.elm-lang.org/debugging/)
- [Articolo su quando utilizzare la stampa dei messaggi di debug](https://medium.com/@jono_bruce/when-to-use-debug-log-in-elm-a8a5dab57c39)
- [Video tutorial su come utilizzare Debug.log in Elm](https://youtu.be/pPhGXZVIeF8)