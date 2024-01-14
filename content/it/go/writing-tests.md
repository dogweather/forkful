---
title:                "Go: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-tests.md"
---

{{< edit_this_page >}}

## Perché
Scrivere test è un'importante pratica di sviluppo del software che aiuta a garantire che il codice sia affidabile, robusto e di alta qualità. Inoltre, i test automatizzati aiutano a facilitare la manutenzione del codice e a identificare eventuali bug o problematiche prima che vengano incontrate dagli utenti finali.

## Come
Scrivere test in Go è semplice grazie alle sue potenti funzionalità di testing integrato nel linguaggio. Basta seguire questi semplici passaggi per iniziare:

1. Definire le funzioni di test utilizzando il prefisso "Test" seguito dal nome della funzione da testare.
2. Utilizzare la funzione "t.Errorf()" per segnalare un errore durante il test.
3. Utilizzare la funzione "t.Run()" per suddividere i test in sottogruppi.
4. Eseguire i test utilizzando il comando "go test".

Ecco un esempio di come potrebbe apparire un test scritto in Go:

```Go
func TestSomma(t *testing.T) {
  // Testiamo la funzione di somma per valori positivi
  risultato := Somma(5, 10)
  if risultato != 15 {
    t.Errorf("Somma incorretta, ci aspettavamo 15 ma abbiamo ottenuto %d", risultato)
  }
}

func TestDivisione(t *testing.T) {
  // Testiamo la funzione di divisione per valori negativi
  risultato := Divisione(10, -2)
  if risultato != -5 {
    t.Errorf("Divisione incorretta, ci aspettavamo -5 ma abbiamo ottenuto %d", risultato)
  }
}
```

L'output di questi test dovrebbe essere il seguente:

```
--- PASS: TestSomma (0.00s)
--- PASS: TestDivisione (0.00s)
PASS
ok	com/example/test	0.001s
```

## Deep Dive
Oltre ai semplici test di esempio mostrati sopra, Go offre molte altre funzionalità per scrivere test efficaci e comprensibili. Alcune di queste includono:

- Utilizzo delle funzioni di utilità "t.Helper()" per identificare in modo chiaro quali chiamate di funzione stanno causando eventuali errori.
- Utilizzo delle funzioni di benchmarking per testare le prestazioni del codice.
- Utilizzo delle funzioni di mocking per testare il codice in isolamento senza dipendere da altre funzioni o librerie.

Scopri di più su come scrivere test efficaci in Go dalla documentazione ufficiale e dalla community di Go.

## Vedi Anche
- [Documentazione ufficiale di Go sul testing](https://golang.org/pkg/testing/)
- [Go in Action: Writing Effective Unit Tests](https://www.manning.com/books/go-in-action-second-edition)
- [Effective Go: Testing](https://golang.org/doc/effective_go.html#testing)