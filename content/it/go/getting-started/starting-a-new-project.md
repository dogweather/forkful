---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:45.724392-07:00
description: "Avviare un nuovo progetto in Go comporta l'impostazione di uno spazio\
  \ di lavoro e l'inizializzazione dello stesso con i moduli Go necessari. I\u2026"
lastmod: '2024-03-11T00:14:16.458540-06:00'
model: gpt-4-0125-preview
summary: "Avviare un nuovo progetto in Go comporta l'impostazione di uno spazio di\
  \ lavoro e l'inizializzazione dello stesso con i moduli Go necessari. I\u2026"
title: Iniziare un nuovo progetto
---

{{< edit_this_page >}}

## Cosa & Perché?

Avviare un nuovo progetto in Go comporta l'impostazione di uno spazio di lavoro e l'inizializzazione dello stesso con i moduli Go necessari. I programmatori fanno ciò per organizzare il codice, gestire efficacemente le dipendenze e facilitare i processi di build. È fondamentale per creare software scalabile e mantenibile in Go.

## Come fare:

Prima di tutto, assicurati di avere Go installato eseguendo `go version` nel tuo terminale. Dovresti vedere come output la versione di Go che hai installato. Successivamente, iniziamo un nuovo progetto. Naviga nel tuo spazio di lavoro ed esegui:

```shell
mkdir hello-world
cd hello-world
```

Questo crea e ti sposta in una nuova directory per il tuo progetto. Ora, inizializza il modulo:

```shell
go mod init example.com/hello-world
```

Sostituisci `example.com/hello-world` con il percorso del tuo modulo. Questo comando crea un file `go.mod` nella tua directory, segnalando l'avvio di un nuovo modulo Go. Ecco come potrebbe apparire `go.mod`:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` tiene traccia delle dipendenze del tuo progetto. Ora, crea un file `main.go`:

```shell
touch main.go
```

Apri `main.go` nel tuo editor preferito e aggiungi il seguente codice per stampare "Hello, World!":

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

Per eseguire il tuo programma, torna al terminale ed esegui:

```shell
go run main.go
```

Dovresti vedere:

```plaintext
Hello, World!
```

Congratulazioni! Hai appena avviato un nuovo progetto Go ed eseguito il tuo primo programma Go.

## Approfondimento

L'iniziativa di introdurre i moduli come standard per la gestione delle dipendenze in Go ha rappresentato un cambiamento significativo nell'ecosistema Go, ufficialmente adottato in Go 1.11. Prima dei moduli, gli sviluppatori Go si affidavano alla variabile d'ambiente GOPATH per gestire le dipendenze, che era meno intuitiva e spesso portava al famoso "inferno delle dipendenze".

I moduli forniscono un modo encapsulato per gestire le dipendenze del progetto, la versione, e rappresentano un passo avanti verso la realizzazione di progetti Go più autonomi e portabili. Ogni modulo specifica le proprie dipendenze che Go tiene traccia nel file `go.mod`, semplificando la gestione delle dipendenze in diversi ambienti e fasi di sviluppo.

Tuttavia, vale la pena notare che, sebbene ora i moduli Go siano lo standard, alcuni progetti legacy potrebbero ancora utilizzare GOPATH. Per la maggior parte dei nuovi progetti, i moduli offrono un sistema di gestione più semplice ed efficace, ma comprendere GOPATH può essere utile per mantenere o contribuire a codebase Go più vecchi.

In termini di alternative, mentre i moduli Go sono ora lo standard de facto, la comunità Go ha sperimentato in passato con altri strumenti di gestione delle dipendenze come `dep`. Tuttavia, questi sono stati in gran parte superati dal supporto ufficiale dei moduli integrato nella toolchain Go.
