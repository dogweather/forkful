---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Leggere un file di testo significa estrarre e interpretare i dati contenuti all'interno del file. Questo è un'azione comune nel campo della programmazione, utilizzata per vari scopi come il salvataggio o la condivisione di dati.

## Come si fa:

Ecco un esempio semplice di come leggere un file di testo in Go.
```Go
package main
import ("io/ioutil"
        "fmt")

func main() {
    data, err := ioutil.ReadFile("test.txt")
    
    if err != nil {
        fmt.Println("File reading error", err)
        return
    }
    
    fmt.Println("Contents of file:", string(data))
}
```
Se "test.txt" contiene le parole "Ciao Mondo", l'output sarà:
```Go
Contents of file: Ciao Mondo
```
## Approfondimenti:

Dal punto di vista storico, i file di testo sono stati sin dall'inizio un mezzo per archiviare e condividere dati. Gli altri metodi per leggere i file includono l'uso di Scanner e Bufio in Go, che possono essere più efficaci per i file di grandi dimensioni. Tuttavia, ioutil.ReadFile è più pratico per i file più piccoli, poiché legge il file completo in memoria in un colpo solo.

## Guarda Anche:

Per ulteriori informazioni, consultare le seguenti risorse:

1. [Package io - The Go Programming Language - golang.org](https://golang.org/pkg/io/)
2. [Reading Files in Go - Go by Example - gobyexample.com](https://gobyexample.com/reading-files)
3. [Working with Files in Go - The Go Programming Language Blog - blog.golang.org](https://blog.golang.org/go1.16-embed-iofs)