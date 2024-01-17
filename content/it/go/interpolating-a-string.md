---
title:                "Interpolazione di una stringa"
html_title:           "Go: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

Cosa & perché?
Interpolare una stringa è un processo che ti permette di inserire delle variabili all'interno di una stringa, in modo da rendere il tuo codice più dinamico e flessibile. I programmatori spesso utilizzano questa tecnica per creare messaggi personalizzati o per formattare i dati in modo più chiaro.

Come fare:
```Go
package main

import "fmt"

func main() {
	name := "Marco"
	age := 30
	fmt.Printf("Ciao, mi chiamo %s e ho %d anni.", name, age)
}

```
Output:
```Ciao, mi chiamo Marco e ho 30 anni.```

Deep Dive:
Interpolare una stringa è una tecnica comune utilizzata in diversi linguaggi di programmazione, tra cui Go. È stata introdotta per la prima volta nel linguaggio di programmazione SNOBOL nel 1962 e poi adottata in altri linguaggi come C, Perl e Java. Un'alternativa all'interpolazione è la concatenazione di stringhe, che può diventare molto complicata e disorganizzata quando ci sono molte variabili da inserire.

Un'altra considerazione nell'interpolazione è la formattazione dei dati. In Go, puoi utilizzare il verbo "%v" per formattare le variabili, ma esistono anche altri verbi come "%d" per numeri interi e "%s" per stringhe.

Vediamo un altro esempio:
```Go
package main

import "fmt"

func main() {
	city := "Milano"
	temperature := 26.5
	fmt.Printf("Oggi a %s ci sono %0.1f gradi.", city, temperature)
}
```
Output:
```Oggi a Milano ci sono 26.5 gradi.```

See Also:
- Documentazione di Go sull'interpolazione di stringhe: https://gobyexample.com/string-formatting
- Un tutorial su come utilizzare l'interpolazione in Go: https://www.digitalocean.com/community/tutorials/how-to-use-string-formatters-in-go
- Interpolazione di stringhe VS concatenazione: https://www.educative.io/edpresso/interpolation-vs-concatenation-in-go