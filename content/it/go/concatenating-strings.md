---
title:                "Concatenazione di stringhe"
html_title:           "Go: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Una delle operazioni più comuni in programmazione è la concatenazione di stringhe, ovvero l'aggiunta di una stringa a un'altra. Questo è spesso utile quando si vuole creare un output più complesso, come un messaggio personalizzato o un documento di testo.

## Come Fare

```Go
package main

import "fmt"

func main() {
    greeting := "Ciao,"
    name := "Mario"
    message := greeting + " " + name // concatenazione delle stringhe
    fmt.Println(message) 
}
```

Questo codice produce l'output "Ciao, Mario". Come si vede nell'esempio, per concatenare le stringhe in Go si utilizza il simbolo "+" tra di esse. È possibile anche concatenare più stringhe in una sola istruzione, semplicemente aggiungendo un altro simbolo "+" tra due nuove stringhe.

Qui di seguito è un altro esempio di concatenazione di stringhe che utilizza la funzione `fmt.Sprintf()` per formattare una stringa utilizzando variabili definite in precedenza:

```Go
package main

import "fmt"

func main() {
    age := 35
    message := fmt.Sprintf("Mario ha %d anni", age) // formattazione con variabili
    fmt.Println(message)
}
```

Questo codice produce l'output "Mario ha 35 anni".

## Approfondimento

In Go, le stringhe sono tipi di dato immutabili, il che significa che una volta definite non possono essere modificate. Per questo motivo, ogni volta che si effettua una concatenazione di stringhe, ne viene creata una nuova, piuttosto che modificarne una esistente. Questo può influire sulle prestazioni in caso di concatenazioni ripetute di stringhe molto lunghe.

Per evitare questo problema, in Go esiste il pacchetto `strings` che fornisce funzioni apposite per manipolare le stringhe, tra cui la funzione `Join()` che consente di concatenare una slice di stringhe senza creare nuove variabili. Ecco un esempio:

```Go
package main

import "fmt"
import "strings"

func main() {
    names := []string{"Mario", "Luigi", "Principessa Peach"}
    message := strings.Join(names, ", ") // join di una slice di stringhe
    fmt.Println("Saluti da", message)
}
```

Questo codice produrrà l'output "Saluti da Mario, Luigi, Principessa Peach". In questo caso la funzione `Join()` unisce le stringhe della slice utilizzando la virgola come separatore.

## Vedi Anche

- [Documentazione ufficiale di Go sulle stringhe](https://golang.org/pkg/strings/)
- [Altre funzioni utili del pacchetto `strings`](https://www.programming-books.io/essential/go/joining-strings-b940abf041e44c5e8547a202321d1fb2)