---
title:    "Go: Leggere gli argomenti della riga di comando"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

I parametri della riga di comando sono un aspetto importante della programmazione in Go. Essi consentono di fornire input per il programma direttamente dall'interfaccia utente, rendendolo più versatile e interattivo.

## Come Fare

Per leggere i parametri della riga di comando in Go, è necessario importare il pacchetto `os`. Successivamente, è possibile utilizzare la funzione `os.Args` per ottenere un array contenente tutti i parametri inseriti dall'utente. Di seguito un esempio di codice:

```Go
import "os"

func main() {
  fmt.Println("Parametri inseriti:", os.Args[1:])
}
```

L'output di questo codice sarà una lista di tutti i parametri inseriti dall'utente dopo il nome del programma. Ad esempio, se il programma si chiama "hello" e viene lanciato con il comando `hello ciao`, l'output sarà `Parametri inseriti: [ciao]`.

## Approfondimento

Oltre alla funzione `os.Args`, esiste anche il pacchetto `flag` che fornisce funzionalità più avanzate per la lettura dei parametri della riga di comando. Esso permette di specificare i tipi di dati dei parametri e di fornire valori di default in caso di mancata specifica da parte dell'utente. Inoltre, il pacchetto `flag` permette di utilizzare alias per i parametri e di gestirne la validità attraverso i flag `Bool`, `Int`, `String` e così via.

## Vedi Anche

- Documentazione ufficiale sul pacchetto `os` e `flag`: https://golang.org/pkg/os/ e https://golang.org/pkg/flag/