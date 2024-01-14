---
title:                "Go: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se hai mai lavorato con file di testo in Go, sai quanto sia importante saperli leggere correttamente. In questo post, esploreremo come leggere un file di testo utilizzando il linguaggio di programmazione Go e perché è un'abilità fondamentale per ogni sviluppatore.

## Come fare

Per leggere un file di testo utilizzando Go, dobbiamo prima creare un oggetto di tipo *os.File* che rappresenta il nostro file di testo. Possiamo farlo utilizzando la funzione *os.Open* e passando come argomento il percorso del nostro file di testo.

Una volta creato l'oggetto file, possiamo utilizzare il metodo *bufio.NewScanner* per creare uno scanner che ci permette di leggere il contenuto del file riga per riga. Inoltre, possiamo utilizzare il metodo *scanner.Text()* per ottenere il testo della riga corrente.

Di seguito è riportato un esempio di come leggere un file di testo utilizzando Go:

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("test.txt")
	if err != nil {
		fmt.Println("Errore durante l'apertura del file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	// leggi il contenuto del file riga per riga
	for scanner.Scan() {
		// otteniamo il testo della riga attuale
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Errore durante la scansionamento del file:", err)
		return
	}
}
```

Se il file di testo contiene il seguente testo:

```
Ciao a tutti!
Questo è un esempio di file di testo
con più righe di contenuto.
```

L'output del programma sarà:

```
Ciao a tutti!
Questo è un esempio di file di testo
con più righe di contenuto.
```

## Approfondimento

Oltre al metodo *scanner.Text()*, esistono anche altri metodi utili per leggere un file di testo utilizzando Go:

- *scanned.Bytes()* restituisce i byte della riga corrente
- *scanner.Scan()* ci dice se ci sono ancora righe da leggere
- *scanner.Err()* restituisce un errore se ce ne sono stati durante la lettura del file

Inoltre, possiamo utilizzare anche il pacchetto *ioutil* per leggere l'intero contenuto del file di testo in una sola volta utilizzando il metodo *ioutil.ReadFile*. Tieni presente che questa soluzione è adatta solo per file di testo di piccole dimensioni.

## Vedi anche

- [Documentazione ufficiale di Go sulla lettura di file di testo](https://golang.org/pkg/os/)
- [Esempi di codice su Github](https://github.com/search?q=go+read+file)
- [Tutorial su Medium](https://medium.com/@jcox250/read-files-with-golang-32773c12690e)