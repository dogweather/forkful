---
title:                "Utilizzare le espressioni regolari."
html_title:           "Go: Utilizzare le espressioni regolari."
simple_title:         "Utilizzare le espressioni regolari."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

Se sei un programmatore, è probabile che tu abbia già sentito parlare delle espressioni regolari. Ma cosa sono e perché sono così importanti? Scopriamolo insieme!

## Cosa e perché?
Le espressioni regolari sono un modo per cercare e manipolare testo in modo efficiente. Sono spesso utilizzate da programmatori per elaborare grandi quantità di dati o per ribareggiare parti di un testo. 

Ma perché dovresti preoccuparti di imparare a utilizzare le espressioni regolari? Semplice. Una volta padroneggiato il loro utilizzo, puoi risparmiare tempo e fatica nello svolgimento di task ripetitivi e complessi. Inoltre, le espressioni regolari sono supportate da diversi linguaggi di programmazione, compreso il nostro amato Go!

## Come fare:
Ecco un esempio di codice Go per cercare una parola specifica in una stringa e sostituirla con un'altra:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	
	// Dichiarazione della stringa
	str := "Ciao, il mio nome è Mario! Ma puoi chiamarmi SuperMario."
	
	// Definizione dell'espressione regolare per cercare "Mario"
	exp := regexp.MustCompile("Mario")
	
	// Sostituzione della parola "Mario" con "Luigi"
	str = exp.ReplaceAllString(str, "Luigi")
	
	// Stampa del risultato
	fmt.Println(str)
}
```

L'output di questo codice sarà:

```
Ciao, il mio nome è Luigi! Ma puoi chiamarmi SuperLuigi.
```

## Profondità di campo:
Le espressioni regolari sono state originariamente sviluppate da matematici e logici nel 1950, ma hanno trovato molta popolarità tra i programmatori negli anni '70 e '80. Nei tempi moderni, ci sono molte alternative alle espressioni regolari, tra cui la libreria "strings" di Go che fornisce funzioni simili.

Tuttavia, le espressioni regolari rimangono una parte importante nello sviluppo di software, soprattutto quando si lavora con testo strutturato o complesso. Se vuoi saperne di più su come utilizzarle in dettaglio, puoi consultare la documentazione ufficiale di Go o cercare online per tutorial e guide dettagliate.

## Vedi anche:
- Documentazione ufficiale di Go su espressioni regolari: https://golang.org/pkg/regexp/
- Un tutorial su espressioni regolari in Go: https://www.regular-expressions.info/go.html
- Un'altra guida pratica su come utilizzare espressioni regolari in Go: https://www.oreilly.com/learning/regular-expressions-in-go