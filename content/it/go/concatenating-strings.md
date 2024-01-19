---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?
La concatenazione di stringhe è il processo di unione di due o più stringhe in un’unica stringa. Ciò è utile per i programmatori quando si tratta di manipolare e presentare dati testuali in modi specifici.

## Come:
In Go, possiamo concatenare stringhe utilizzando l'operatore "+". Vediamo alcuni esempi:

```Go
pacchetto principale

import "fmt"

func main() {
    var string1 = "Ciao, "
    var string2 = "come stai?"
    
    fmt.Println(string1 + string2)
}
```
Questa stampa: "Ciao, come stai?" sul terminale.

Alternativamente, si può utilizzare la funzione Sprintf del pacchetto fmt, che consente la formattazione delle stringhe:

```Go
pacchetto principale

import "fmt"

func main() {
    var string1 = "Ciao"
    var string2 = "come stai?"
    
    risultato := fmt.Sprintf("%s, %s", string1, string2)
    fmt.Println(risultato)
}
```
Uscita: "Ciao, come stai?"

## Approfondimento
Un po' di contesto storico: il linguaggio di programmazione Go, sviluppato presso Google, ha cercato di rendere la manipolazione delle stringhe più efficiente evitando la creazione di nuovi oggetti stringa ogni volta che si concatenano stringhe.

Alternative: invece della concatenazione di stringhe, si possono utilizzare byte e Rune in Go per manipolare stringhe, ma questo va oltre lo scopo di questo articolo.

Dettagli di implementazione: in Go, la stringa è immutabile. Ciò significa che non è possibile modificare una stringa una volta creata. Quando concateniamo stringhe, creiamo effettivamente una nuova stringa.

## Vedi Anche 
Per ulteriori informazioni, visita questi link:
- Documenti Go su Stringhe: https://golang.org/ref/spec#String_types
- Esempi di concatenazione di Stringhe Go: https://gobyexample.com/string-formatting