---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:06.553876-07:00
description: "L'uso di un debugger nella programmazione Go implica l'impiego di strumenti\
  \ o funzionalit\xE0 per ispezionare e modificare lo stato di un programma in\u2026"
lastmod: '2024-03-13T22:44:42.913213-06:00'
model: gpt-4-0125-preview
summary: "L'uso di un debugger nella programmazione Go implica l'impiego di strumenti\
  \ o funzionalit\xE0 per ispezionare e modificare lo stato di un programma in\u2026"
title: Utilizzare un debugger
---

## Come fare:
Go fornisce una struttura integrata per il debug chiamata `delve`. Si tratta di uno strumento di debug completo che consente di eseguire programmi Go passo dopo passo, ispezionare le variabili del programma e valutare espressioni.

Per iniziare, devi prima installare `delve`. Puoi farlo eseguendo:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Ora, vediamo di eseguire il debug di un semplice programma Go. Considera un programma `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

Per avviare il debug di questo programma, apri un terminale nella directory del progetto ed esegui:

```shell
dlv debug
```

Questo comando compila il programma disabilitando le ottimizzazioni (per migliorare l'esperienza di debug), lo avvia e collega un debugger ad esso.

Una volta che `delve` è in esecuzione, ti trovi nella shell interattiva del debugger. Ecco alcuni comandi di base:

- `break main.main` imposta un breakpoint nella funzione `main`.
- `continue` riprende l'esecuzione del programma fino al raggiungimento di un breakpoint.
- `print message` stamperà il valore della variabile `message`.
- `next` fa avanzare l'esecuzione del programma alla riga successiva.
- `quit` esce dal debugger.

L'output al raggiungimento del breakpoint e alla stampa della variabile potrebbe apparire così:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

Usando questi comandi, puoi procedere passo dopo passo attraverso il tuo programma, ispezionandone lo stato man mano per capire come si comporta e identificare eventuali problemi.

## Approfondimento
La scelta di `delve` come strumento di debug di elezione per Go rispetto a strumenti tradizionali come GDB (GNU Debugger) è dovuta principalmente alla natura del modello di esecuzione e del runtime di Go. GDB non è stato inizialmente progettato tenendo a mente il runtime di Go, rendendo `delve` una scelta più adatta per gli sviluppatori Go. `Delve` è specificamente progettato per Go, offrendo un'esperienza di debug più intuitiva per le goroutine, i canali e altre costruzioni specifiche di Go.

Inoltre, `delve` supporta un'ampia gamma di funzionalità oltre a quelle offerte da GDB di base quando si lavora con programmi Go. Queste includono, ma non si limitano a: l'attaccamento a processi in esecuzione per il debug; breakpoint condizionali; e la valutazione di espressioni complesse che possono coinvolgere i primitivi di concorrenza di Go.

Mentre `delve` è il debugger di riferimento per molti sviluppatori Go, vale la pena notare che la toolchain di Go include anche forme di supporto al debug più leggere, come lo strumento integrato `pprof` per il profiling e lo strumento `trace` per la visualizzazione della concorrenza. Questi strumenti possono talvolta fornire un modo più veloce o ad alto livello per diagnosticare problemi di prestazioni del programma o bug di concorrenza, che potrebbe essere complementare o addirittura preferibile a seconda del contesto di debug.
