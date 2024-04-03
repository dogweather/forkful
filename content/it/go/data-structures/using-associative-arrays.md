---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:56.616038-07:00
description: "Gli array associativi, noti come mappe in Go, ti permettono di memorizzare\
  \ coppie chiave-valore dove ogni chiave unica \xE8 mappata a un valore. I\u2026"
lastmod: '2024-03-13T22:44:42.900234-06:00'
model: gpt-4-0125-preview
summary: "Gli array associativi, noti come mappe in Go, ti permettono di memorizzare\
  \ coppie chiave-valore dove ogni chiave unica \xE8 mappata a un valore."
title: Utilizzo di array associativi
weight: 15
---

## Come fare:
Creare e inizializzare una mappa in Go può essere fatto in vari modi. Ecco un esempio basilare per iniziare:

```go
package main

import "fmt"

func main() {
    // Dichiarare e inizializzare una mappa
    colors := map[string]string{
        "rosso":   "#FF0000",
        "verde": "#00FF00",
        "blu":  "#0000FF",
    }

    fmt.Println(colors)
    // Output: map[blu:#0000FF verde:#00FF00 rosso:#FF0000]
}
```

Per aggiungere o aggiornare elementi, assegni un valore a una chiave in questo modo:

```go
colors["bianco"] = "#FFFFFF"
fmt.Println(colors)
// Output: map[blu:#0000FF verde:#00FF00 rosso:#FF0000 bianco:#FFFFFF]
```

Accedere a un valore tramite la sua chiave è semplice:

```go
fmt.Println("Il codice esadecimale per il rosso è:", colors["rosso"])
// Output: Il codice esadecimale per il rosso è: #FF0000
```

Per eliminare un elemento, utilizzare la funzione `delete`:

```go
delete(colors, "rosso")
fmt.Println(colors)
// Output: map[blu:#0000FF verde:#00FF00 bianco:#FFFFFF]
```

Iterare su una mappa si esegue utilizzando un ciclo for:

```go
for color, hex := range colors {
    fmt.Printf("Chiave: %s Valore: %s\n", color, hex)
}
```

Ricorda, le mappe in Go non sono ordinate. L'ordine di iterazione non è garantito.

## Approfondimento
In Go, le mappe sono implementate come tabelle hash. Ogni entrata nella mappa consiste di due elementi: una chiave e un valore. La chiave è hashata per memorizzare l'entrata, il che permette operazioni in tempo costante per un piccolo insieme di dati e complessità temporale media di O(1) con un hashing adeguato, che può degradare a O(n) nel caso peggiore con molte collisioni hash.

Un'annotazione significativa per i nuovi programmatori di Go è che i tipi mappa sono tipi di riferimento. Questo significa che quando passi una mappa a una funzione, qualsiasi modifica apportata alla mappa all'interno di quella funzione è visibile al chiamante. Questo è diverso, per esempio, dal passare una struct a una funzione, dove la struct viene copiata a meno che non venga passata tramite un puntatore.

Sebbene le mappe siano incredibilmente versatili ed efficienti per la maggior parte dei casi d'uso che coinvolgono array associativi, in applicazioni critici per le prestazioni, può essere vantaggioso utilizzare strutture dati con caratteristiche di prestazione più prevedibili, specialmente se le distribuzioni chiave possono causare frequenti collisioni.

Un'altra alternativa da considerare è la `sync.Map`, disponibile da Go 1.9, progettata per casi d'uso in cui le chiavi sono scritte una sola volta ma lette molte volte, offrendo miglioramenti dell'efficienza in questi scenari. Tuttavia, per le applicazioni Go convenzionali, l'uso della mappa regolare è idiomatico e spesso l'approccio consigliato per la sua semplicità e il supporto diretto nel linguaggio.
