---
title:                "Utilizzo di un debugger"
aliases:
- /it/clojure/using-a-debugger.md
date:                  2024-01-26T03:48:11.517778-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Usare un debugger significa equipaggiarsi con una lente d'ingrandimento per scrutare il proprio codice. I programmatori fanno ciò per schiacciare i bug, comprendere il flusso e assicurarsi che la loro logica si svolga come previsto.

## Come fare:
Clojure si appoggia alla Java Virtual Machine (JVM), quindi molte operazioni di debug avvengono con strumenti Java. Uno di questi strumenti è `CIDER`, un pacchetto molto potente per lo sviluppo in Clojure su Emacs, che ha solide capacità di debugging. Facciamo un tuffo:

```clojure
;; Prima, collegati a un progetto Clojure all'interno di Emacs usando CIDER
M-x cider-jack-in

;; Imposta un breakpoint
;; Naviga alla riga nel tuo codice Clojure che vuoi ispezionare e
;; premi "C-c M-b" o esegui:
M-x cider-debug-defun-at-point

;; Quando il codice viene eseguito, incontrerai il breakpoint. CIDER ti chiederà:
;; 1. n per andare al prossimo passo logico nell'esecuzione,
;; 2. c per continuare l'esecuzione fino al prossimo breakpoint,
;; 3. q per uscire dal debugging.

;; Ispeziona le variabili locali al breakpoint
;; Mentre sei in un breakpoint, digita:
locals

;; Vedrai una lista di variabili locali e i loro valori stampati nel minibuffer.
```
Un output di esempio potrebbe essere:
```clojure
{:x 10, :y 20, :result 200}
```

## Approfondimento
Il debugger è uno strumento vecchio quanto il mondo in termini informatici. Il termine "bug" fu coniato nei primi giorni dell'informatica quando un insetto reale causò un errore cortocircuitando una macchina.

Anche se `CIDER` è ottimo per gli appassionati di Emacs, ci sono alternative per il debugging in Clojure. Per esempio, usare IntelliJ con il plugin Cursive può offrire un'esperienza di debugging più basata su GUI. Inoltre, puoi utilizzare il built-in Leiningen o tools.deps per controllare il flusso del processo durante il debugging.

Sotto il cofano, questi debugger manipolano spesso bytecode, eseguono valutazioni in sessioni nREPL dedicate e offrono l'ispezione dello stack trace. Stanno sfruttando le capacità della JVM sottostante, attingendo alla ricchezza dei framework di debugging di Java.

## Vedi Anche
- [Documentazione del Debugger CIDER](https://docs.cider.mx/cider/debugging/debugger.html)
- [Debugger Cursive](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen per Automazione e Debugging](https://leiningen.org/)
- [tools.deps.alpha per più controllo](https://github.com/clojure/tools.deps.alpha)
