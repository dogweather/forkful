---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Creare un file temporaneo consiste nell'elaborare dati temporanei senza influire sui dati principali. È uno strumento essenziale per i programmatori, utile per testare codici, eseguire backup e gestire file di grandi dimensioni senza intasare la memoria.

## Come fare:
Ecco un esempio di come creare un file temporaneo in Go. Assicuratevi di aver importato correttamente il pacchetto `io/ioutil` per utilizzare la funzione `TempFile`.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

func main() {
	tempFile, err := ioutil.TempFile("", "sample")
	
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(tempFile.Name())

	defer os.Remove(tempFile.Name())
}
```
Questo creerà un file temporaneo chiamato 'sample' e ne stamperà il percorso completo. Dopo l'uso, il file viene rimosso con `defer os.Remove(tempFile.Name())`.

## Approfondimento
Creare file temporanei non è una novità nella programmazione. L'uso di file temporanei risale ai primi giorni di Unix, dove erano usati per conservare i dati tra diverse fasi di un pipeline di programmi.

Un'alternativa alla creazione di file temporanei in Go può essere l'uso di una `map` o di uno `slice` per conservare i dati temporanei nella memoria, ma questi metodi sono meno efficaci con grandi quantità di dati.

Quando si crea un file temporaneo in Go, viene creata una directory temporanea nel filesystem. L'esatta posizione di questa directory può variare a seconda del sistema operativo.

## Altre fonti
Per saperne di più sulle funzioni `ioutil.TempFile` e `os.Remove`, dai un'occhiata alla documentazione ufficiale di Go:

[ioutil.TempFile](https://pkg.go.dev/io/ioutil#TempFile)

[os.Remove](https://pkg.go.dev/os#Remove)

E per un approfondimento generale sulla gestione dei file in Go, consulta questo link:

[File handling in Go](https://golangbot.com/write-files/)