---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:20.081776-07:00
description: "L'interpolazione di stringhe \xE8 un metodo per costruire stringhe che\
  \ incorporano variabili, consentendo la creazione di stringhe dinamiche. I programmatori\u2026"
lastmod: '2024-02-25T18:49:40.823634-07:00'
model: gpt-4-0125-preview
summary: "L'interpolazione di stringhe \xE8 un metodo per costruire stringhe che incorporano\
  \ variabili, consentendo la creazione di stringhe dinamiche. I programmatori\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## Cosa & Perché?

L'interpolazione di stringhe è un metodo per costruire stringhe che incorporano variabili, consentendo la creazione di stringhe dinamiche. I programmatori fanno ciò per personalizzare messaggi, costruire URL, creare query SQL e altro, permettendo così di ottenere codice più leggibile e manutenibile.

## Come fare:

In Go, l'interpolazione di stringhe si ottiene comunemente utilizzando il pacchetto `fmt`, in particolare con la funzione `Sprintf`, che ti permette di iniettare variabili in una stringa specificando verbi di formattazione. I verbi sono segnaposto nella stringa di formato e vengono sostituiti dai valori delle variabili fornite. Ecco come si utilizza:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Utilizzando Sprintf per l'interpolazione di stringhe
    message := fmt.Sprintf("Ciao, mi chiamo %s e ho %d anni.", name, age)
    fmt.Println(message) // Output: Ciao, mi chiamo Jane e ho 28 anni.
}
```

Si noti che `%s` viene usato per le stringhe, e `%d` per gli interi. La documentazione del pacchetto `fmt` fornisce un elenco completo dei verbi di formattazione per diversi tipi di dati.

## Approfondimento

Il concetto di interpolazione delle stringhe esiste in molti linguaggi di programmazione, sebbene con sintassi e capacità diverse. In Go, mentre la funzione `Sprintf` del pacchetto `fmt` è l'approccio più comunemente utilizzato, potrebbe non essere sempre il più efficiente, specialmente per concatenazioni semplici o quando si lavora con codice altamente sensibile alle prestazioni.

Il pacchetto `fmt` utilizza la riflessione per interpretare dinamicamente i tipi delle variabili a runtime, il che, sebbene flessibile, comporta un sovraccarico. Per scenari in cui la prestazione è critica, la concatenazione diretta di stringhe o il tipo `strings.Builder` possono offrire alternative migliori. La concatenazione diretta è semplice ma può diventare ingombrante con molteplici variabili. `strings.Builder`, d'altra parte, fornisce un modo più performante e leggibile per costruire stringhe complesse in un ciclo o quando si ha a che fare con molte variabili:

```go
var sb strings.Builder
sb.WriteString("Ciao, mi chiamo ")
sb.WriteString(name)
sb.WriteString(" e ho ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" anni.")
message := sb.String()

fmt.Println(message) // Produce lo stesso output di prima
```

In definitiva, la scelta tra `fmt.Sprintf`, la concatenazione diretta e `strings.Builder` dipende dai requisiti specifici della tua applicazione, come la complessità della stringa che viene costruita e le considerazioni sulle prestazioni.
