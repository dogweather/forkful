---
title:    "Go: Stampa dell'output di debug"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Scrivere del codice può essere un processo complesso e a volte è difficile capire cosa sta realmente accadendo durante l'esecuzione del programma. È qui che la stampa del debug output diventa utile. Questo strumento consente ai programmatori di visualizzare informazioni sullo stato del programma e aiuta a individuare eventuali errori o problemi.

## Come fare

Per stampare il debug output in Go, possiamo utilizzare la funzione `fmt.Printf()`. Questa funzione accetta una stringa formattata e una serie di valori che verranno inseriti nella stringa, in base alle specifiche di formattazione. Ecco un esempio di codice che stampa una variabile di tipo intero:

```Go
package main

import "fmt"

func main() {
    num := 10
    fmt.Printf("Il numero è %d\n", num)
}
```

L'output di questo codice sarà:

```
Il numero è 10
```

Possiamo anche utilizzare la funzione `fmt.Println()` per stampare il debug output direttamente senza specificare alcun formato. Questa funzione aggiunge automaticamente un carattere di nuova riga alla fine dell'output. Ecco un altro esempio:

```Go
package main

import "fmt"

func main() {
    nome := "Mario"
    cognome := "Rossi"
    fmt.Println("Il mio nome è", nome, "e il mio cognome è", cognome)
}
```

L'output di questo codice sarà:

```
Il mio nome è Mario e il mio cognome è Rossi
```

## Approfondimento

Oltre alle funzioni `fmt.Printf()` e `fmt.Println()`, Go offre anche altre opzioni per la stampa del debug output, come ad esempio la funzione `fmt.Fprintf()` per scrivere su stream specifici (come i file) e la libreria `log` per la gestione dei log. Inoltre, è possibile utilizzare la formattazione dei tipi di dato personalizzata per rendere ancora più dettagliato e leggibile il debug output.

Ricordate però che, come per ogni altra cosa, è importante non esagerare con la stampa del debug output. Utilizzatela solo quando necessario e assicuratevi di rimuoverla dal codice finale.

## Vedi anche

- Documentazione ufficiale di Go sulla formattazione delle stringhe: https://golang.org/pkg/fmt/
- Tutorial su come utilizzare la libreria `log` per la gestione dei log in Go: https://golangbot.com/golang-log-package/
- Guida alla formattazione dei tipi di dato personalizzata in Go: https://www.sohamkamani.com/golang/string-formatting/