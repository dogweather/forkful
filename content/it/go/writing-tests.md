---
title:                "Scrivere test"
html_title:           "Go: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-tests.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Scrivere test è un'attività molto importante per i programmatori di Go (la versione attuale del linguaggio di programmazione). I test sono piccoli programmi che permettono di verificare che il codice scritto funzioni correttamente. In altre parole, sono una sorta di "controllo di qualità" per il nostro codice.

I programmatori scrivono test per garantire che il loro codice non abbia bug o errori, e per assicurarsi che quando fanno delle modifiche al codice, tutto continui a funzionare come previsto.

## Come fare:

Usando il pacchetto di testing integrato in Go, possiamo creare dei file di test separati dal nostro codice. Esempio:

```Go
package nomepacchetto

import "testing" // importiamo il pacchetto di testing in Go

func TestMyFunction(t *testing.T) { // nome della funzione di test
	got := MyFunction(2, 3) // chiamiamo la funzione che vogliamo testare
	want := 5 // risultato che ci aspettiamo dalla funzione
	if got != want { // confrontiamo il risultato ottenuto con quello atteso
		t.Errorf("MyFunction(2, 3) = %d; want %d", got, want)
		// se il test fallisce, stampiamo un messaggio di errore con i risultati
	}
}
``` 

Questa è una semplice funzione di test che mostra come possiamo verificare se la nostra funzione `MyFunction` restituisce il risultato corretto. Se tutto va bene, il test passerà senza errori e possiamo essere sicuri che la nostra funzione funzioni correttamente.

## Deep Dive:

Il concetto di scrivere test è chiamato "test-driven development" (TDD) e in pratica significa scrivere i test prima del codice. Questo approccio è stato introdotto per garantire una maggiore affidabilità del codice e una migliore gestione dei bug.

Ci sono anche altri strumenti per scrivere test in Go, come il pacchetto `gocheck` o `goconvey`, ma il pacchetto di testing integrato è già molto potente e generalmente sufficiente per le necessità dei programmatori di Go.

## See Also:

Vogliamo che il nostro codice sia sempre il migliore possibile, quindi è importante avere delle buone pratiche di testing. Ecco alcuni link utili per ulteriori informazioni sui test in Go:

- [Documentazione ufficiale di Go sul testing](https://golang.org/pkg/testing/)
- [Tutorial su TDD con Go](https://medium.com/@roderickhsiao/test-driven-development-with-go-f7eb4a62a4c8)
- [Articolo su GoLand per scrivere test in modo efficiente](https://blog.jetbrains.com/go/2019/03/11/test-with-goland-tips-and-tricks/)