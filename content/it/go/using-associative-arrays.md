---
title:                "Utilizzo di array associativi"
date:                  2024-01-30T19:11:26.531531-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di array associativi"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi, noti come mappe in Go, ti permettono di memorizzare e accedere ai dati tramite coppie chiave-valore. Sono essenziali per gestire collezioni in cui è possibile cercare valori rapidamente tramite una chiave unica, semplificando la manipolazione e il recupero dei dati nei tuoi programmi.

## Come fare:

In Go, le mappe sono semplici da usare. Ecco una guida semplice per iniziare:

1. **Dichiarare e Inizializzare le Mappe**

```Go
package main

import "fmt"

func main() {
    // Inizializza una mappa vuota con chiavi stringa e valori int
    var scores map[string]int
    fmt.Println(scores) // Stampa: map[]

    // Dichiarare e inizializzare una mappa non vuota
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // Stampa: map[green:#00ff00 red:#ff0000]
}
```

2. **Aggiungere e Accedere agli Elementi**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // Stampa: 5
}
```

3. **Iterare Sulle Mappe**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s fa %s\n", key, value)
    }
    // L'ordine dell'output può variare, poiché le mappe non garantiscono un ordine.
}
```

4. **Eliminare Elementi**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // Prima dell'eliminazione

    delete(meals, "lunch")
    fmt.Println(meals) // Dopo l'eliminazione
}
```

## Approfondimento

Introdotta in Go 1, le mappe forniscono un modo integrato per gestire gli array associativi in modo efficiente. A differenza delle slice, che sono collezioni ordinate, le mappe sono disordinate. Ciò significa che l'ordine di iterazione sugli elementi della mappa non è garantito essere lo stesso tra le esecuzioni, un compromesso per la sua capacità di gestire coppie chiave-valore dinamicamente e con notevole flessibilità.

Sotto il cofano, Go implementa le mappe come tabelle hash, garantendo che la complessità media delle operazioni di accesso, inserimento ed eliminazione sia O(1), nella maggior parte delle circostanze. Tuttavia, vale la pena notare che questa efficienza può variare in base a fattori come le collisioni hash.

Per casi d'uso che richiedono un attraversamento chiave ordinato, potresti considerare di combinare mappe con slice o esplorare pacchetti di terze parti che offrono strutture dati aggiuntive come mappe ordinate o alberi. Nonostante i loro limiti, le mappe di Go sono uno strumento potente ed essenziale per molti scenari di programmazione.
