---
title:                "Lettura degli argomenti da linea di comando"
aliases: - /it/go/reading-command-line-arguments.md
date:                  2024-02-03T18:06:12.760859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lettura degli argomenti da linea di comando"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere gli argomenti da linea di comando in Go implica l'estrazione degli argomenti forniti a un programma durante la sua invocazione dal terminale o dal prompt dei comandi. I programmatori fanno ciò per personalizzare l'esecuzione del programma senza modificare il codice, rendendo le applicazioni più flessibili e orientate all'utente.

## Come fare:

Go offre un accesso diretto agli argomenti della linea di comando tramite il pacchetto `os`, in particolare usando `os.Args`, un array di stringhe. Ecco un semplice esempio per iniziare:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args offre accesso agli argomenti raw della linea di comando
    fmt.Println("Argomenti della linea di comando:", os.Args)

    if len(os.Args) > 1 {
        // Ciclo attraverso gli argomenti, saltando il primo (nome del programma)
        for i, arg := range os.Args[1:] {
            fmt.Printf("Argomento %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("Nessun argomento da linea di comando fornito.")
    }
}
```

Un esempio di output quando eseguito con `go run yourprogram.go arg1 arg2` potrebbe sembrare:

```
Argomenti della linea di comando: [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
Argomento 1: arg1
Argomento 2: arg2
```

Questo stampa tutti gli argomenti inclusi il nome del programma (spesso all'indice 0), poi itera su ogni argomento fornito, stampandoli. Per un parsing degli argomenti più controllato, potresti considerare il pacchetto `flag` per l'analisi delle opzioni della linea di comando.

## Approfondimento

Storicamente, l'accesso agli argomenti della linea di comando è una pratica vecchia quanto la programmazione in C, dove `argc` e `argv[]` servono a uno scopo simile. In Go, `os.Args` rende il processo diretto ma deliberatamente rudimentale. Per scenari più complessi, come la gestione di flag o opzioni, Go offre il pacchetto `flag` che fornisce solide capacità di parsing. Questo potrebbe essere visto come un'alternativa "migliore" quando la tua applicazione richiede più che semplici argomenti posizionali.

A differenza di alcuni linguaggi di scripting che offrono il parsing incorporato degli argomenti della linea di comando in array associativi o oggetti, l'approccio di Go richiede che i programmatori gestiscano manualmente il parsing usando `os.Args` per le esigenze di base o per sfruttare il pacchetto `flag` per scenari più avanzati. Questo design riflette la filosofia di Go di mantenere il linguaggio core semplice fornendo allo stesso tempo potenti librerie standard per compiti comuni. Mentre può introdurre una leggera curva di apprendimento per coloro che sono abituati al parsing incorporato, offre maggiore flessibilità e incoraggia una più profonda comprensione della gestione degli argomenti da linea di comando.
