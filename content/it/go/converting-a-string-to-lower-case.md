---
title:                "Convertire una stringa in minuscolo"
html_title:           "Go: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il processo di conversione di una stringa in lettere minuscole è una comune operazione che i programmatori eseguono per rendere uniforme il testo all'interno del loro codice. Questa pratica è particolarmente utile quando si stanno manipolando dati provenienti da diverse fonti o quando si devono confrontare stringhe senza considerare la distinzione tra maiuscole e minuscole.

## Come fare:

Utilizzando Go, è possibile convertire una stringa in minuscolo utilizzando la funzione "strings.ToLower ()". Ad esempio:

```Go
nome := "MARIO"
fmt.Println(strings.ToLower(nome))
```

L'output di questo codice sarà "mario".

## Approfondimento:

La necessità di convertire le stringhe in minuscolo risale ai primi giorni della programmazione informatica, quando i dati venivano immagazzinati in modo inefficiente e spaziose. Oggi, ci sono alternative come l'utilizzo delle espressioni regolari o l'utilizzo di funzioni built-in specifiche per il linguaggio di programmazione utilizzato.

Con Go, la funzione "strings.ToLower ()" utilizza il package "strings" che contiene una serie di metodi per la manipolazione delle stringhe, compresa la conversione in minuscolo.

## Vedi anche:

Per ulteriori informazioni sulla manipolazione delle stringhe in Go, consulta la documentazione ufficiale del linguaggio: https://golang.org/pkg/strings/

Puoi anche approfondire l'utilizzo delle espressioni regolari nell'articolo "RegEx in Go: A Quick Guide" disponibile su Medium: https://medium.com/rungo/regex-in-go-a-quick-tutorial-363765b9e9fb