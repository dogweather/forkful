---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# Stampa Debug in Go

## Cos'è & Perché?

Stampare debug output significa mostrare la logica e il flusso del tuo programma sul terminale. Lo facciamo per identificare e correggere eventuali problemi o bug in modo rapido ed efficiente.

## Come fare:

Ecco un esempio di come stampare debug output in Go.

```Go
package main

import "log"

func main() {
    log.Print("Stampa di debug in corso ...")
}
```

Output:

```Go
2009/11/10 23:00:00 Stampa di debug in corso ...
```

## Approfondimento:

Molte lingue utilizzano la stampa di output di debug da anni, e Go non fa eccezione. Ci sono alternative presso altre librerie di logging, come golog e zap, che possono offrire più funzionalità. Go utilizza un approccio semplice per la stampa del debug in cui "log.Print" scrive sullo standard error e include la data e l'ora nella stampa.

## Vedi anche:

Per ulteriori informazioni e approfondimenti sul debug in Go, fai riferimento alle seguenti risorse:

1. [Pacchetto log di Go](https://golang.org/pkg/log/): Documentazione ufficiale sul pacchetto log di Go.
2. [Go By Example: Logging](https://gobyexample.com/logging): Un tutorial dettagliato sulla stampa log per il debug in Go.
3. [Logging in Go](https://www.honeybadger.io/blog/golang-logging/): Un blog post approfondito sui diversi metodi di logging in Go.