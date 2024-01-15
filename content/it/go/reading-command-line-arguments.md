---
title:                "Leggere gli argomenti della riga di comando"
html_title:           "Go: Leggere gli argomenti della riga di comando"
simple_title:         "Leggere gli argomenti della riga di comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Leggere gli argomenti della riga di comando è fondamentale per molti programmi Go in quanto consente di passare informazioni esterne al programma stesso. Questo può aiutare a rendere il programma più flessibile e adattabile a diverse situazioni.

## Come fare

Per leggere gli argomenti della riga di comando in Go, è necessario utilizzare il pacchetto "os" e la funzione "Args()". Questo fornirà una slice di stringhe contenente tutti gli argomenti passati al programma dalla riga di comando.

```Go
import "os"

args := os.Args
```

È possibile accedere ai singoli argomenti utilizzando l'indice della slice, ad esempio:

```Go
import "fmt"

fmt.Println("Il primo argomento è:", args[1])
```

L'esempio sopra stamperà il primo argomento passato al programma dalla riga di comando.

## Approfondimento

Ci sono molte utili funzioni nel pacchetto "os" per gestire gli argomenti della riga di comando, tra cui "Len()" per conoscere il numero totale di argomenti passati e "HasPrefix()" per controllare se un determinato argomento inizia con una determinata stringa.

Ecco un esempio di utilizzo delle funzioni di "os" per verificare se il programma è stato chiamato con l'argomento "-help":

```Go
import (
  "os"
  "strings"
)

args := os.Args

for _, arg := range args {
  if strings.HasPrefix(arg, "-help") {
    fmt.Println("Utilizza questo programma in questo modo...")
  }
}
```

Questo esempio illustra come il controllo degli argomenti dalla riga di comando possa aiutare a fornire informazioni o istruzioni all'utente.

## Vedi anche

Per ulteriori informazioni su come gestire gli argomenti della riga di comando in Go, consulta la documentazione ufficiale del linguaggio e il seguente articolo su Medium:

- [Handling Command Line Arguments in Go](https://medium.com/swlh/handling-command-line-arguments-in-go-da4e16b42698) (inglese)