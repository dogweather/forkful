---
title:    "Go: Creazione di un file temporaneo"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché
Creare un file temporaneo è spesso essenziale quando si lavora con i programmi di Go. Permette di gestire i dati in modo efficiente e di evitare confusioni tra processi concorrenti.

## Come fare
Per creare un file temporaneo in Go, si può utilizzare la funzione `ioutil.TempFile ()`. Di seguito è riportato un esempio di codice che crea un file temporaneo e scrive alcune stringhe al suo interno, utilizzando il pacchetto `io/ioutil`.

```
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    tempfile, err := ioutil.TempFile("", "esempio")
    if err != nil {
        fmt.Println("Errore durante la creazione del file temporaneo:", err)
        return
    }
    defer tempfile.Close()

    // Scrivi il contenuto nel file temporaneo
    string1 := []byte("Questo è un testo di esempio")
    string2 := []byte("Questo è un altro testo di esempio")

    if _, err := tempfile.Write(string1); err != nil {
        fmt.Println("Errore durante la scrittura nel file temporaneo:", err)
        return
    }

    if _, err := tempfile.Write(string2); err != nil {
        fmt.Println("Errore durante la scrittura nel file temporaneo:", err)
        return
    }

    fmt.Println("File temporaneo creato con successo:", tempfile.Name())
}
```

Output:

```
File temporaneo creato con successo: /var/folders/q5/lktx6tqd18x3tbb82vxs_w0r0000gn/T/esempio909255921
```

## Approfondimenti
La funzione `ioutil.TempFile()` crea un file temporaneo nella directory specificata dal primo parametro. Se viene passata una stringa vuota, verrà utilizzata la directory predefinita per i file temporanei dell'OS. Il secondo parametro rappresenta invece il prefisso del nome del file temporaneo.

Una volta terminato l'utilizzo del file temporaneo, è importante liberare le risorse utilizzate utilizzando il comando `defer` per chiudere il file.

## Vedi anche
- Documentazione ufficiale di Go su `ioutil.TempFile`: https://golang.org/pkg/io/ioutil/#TempFile
- Un tutorial su come gestire i file temporanei in Go: https://www.digitalocean.com/community/tutorials/golang-working-with-temporary-files