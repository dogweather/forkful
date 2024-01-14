---
title:                "Go: Lettura di un file di testo."
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Perché

Leggere e scrivere dati su un file di testo è una capacità essenziale per qualsiasi programma, in quanto consente di memorizzare e gestire grandi quantità di informazioni senza doverle inserire manualmente ogni volta. In questo post, vedremo come farlo utilizzando il linguaggio di programmazione Go.

##Come fare

Per leggere un file di testo in Go, il primo passo è aprire il file utilizzando la funzione `os.Open()`. Assicurati di controllare gli errori durante l'apertura del file utilizzando l'espressione `defer` e `panic()`. Una volta aperto il file, possiamo utilizzare la funzione `bufio.NewReader()` per creare un buffer di lettura che ci permetta di leggere il file riga per riga. Successivamente, per ogni riga letta, possiamo utilizzare il metodo `ReadString('\n')` per leggere il contenuto della riga e stamparlo a video utilizzando la funzione `fmt.Println()`. Ecco un esempio di codice:

```
package main

import (
  "bufio"
  "fmt"
  "os"
)

func main() {
  // Apertura del file
  file, err := os.Open("test.txt")

  // Controllo degli errori
  defer func() {
    if err != nil {
      panic(err)
    }
  }()

  // Creazione di un buffer di lettura
  scanner := bufio.NewScanner(file)

  // Leggiamo il file riga per riga
  for scanner.Scan() {
    // Leggiamo il contenuto della riga
    line := scanner.Text()
    // Stampiamo il contenuto a video
    fmt.Println(line)
  }

  // Chiudiamo il file
  file.Close()
}
```

Se eseguiamo questo codice su un file di testo con del testo all'interno, otterremo il contenuto del file stampato a video. Ad esempio, se il nostro file si chiama `test.txt` e contiene le seguenti righe:

```
Ciao
Come va?
Tutto bene qui!
```

L'output sarà:

```
Ciao
Come va?
Tutto bene qui!
```

##Approfondimenti

Ci sono molte altre funzioni e metodi disponibili in Go per la lettura di file di testo, come ad esempio i pacchetti "io" e "ioutil". È importante comprenderne le differenze e utilizzare quello più adatto alle tue esigenze. Inoltre, esistono anche delle funzioni per la scrittura su file di testo e per la gestione dei dati strutturati all'interno del file. Per ulteriori informazioni sull'utilizzo di Go per la gestione dei file di testo, consulta la documentazione ufficiale o fai riferimento a risorse online come blog o video tutorial.

##Vedi anche

- Documentazione ufficiale su `os.Open()`: https://golang.org/pkg/os/#Open
- Documentazione ufficiale su `bufio.Scanner`: https://golang.org/pkg/bufio/#Scanner
- Video tutorial su Go e la lettura di file di testo: https://www.youtube.com/watch?v=brGby0ONyTM