---
title:                "Go: Lettura degli argomenti della riga di comando"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché 

La lettura degli argomenti della riga di comando è una delle funzionalità più utili del linguaggio di programmazione Go. Questa capacità consente ai programmatori di creare programmi interattivi, richiedere l'input degli utenti e personalizzare l'esecuzione del codice in base alle informazioni fornite. Continuate a leggere per scoprire come utilizzare questa funzionalità nei vostri progetti Go.

## Come fare 

Per leggere gli argomenti della riga di comando in un programma Go, è necessario utilizzare il pacchetto "flag" incorporato. Iniziate importando il pacchetto con il comando "import flag". Successivamente, create dei flag per i vostri argomenti utilizzando la funzione "flag.String()" o "flag.Bool()", specificando il nome del flag e un valore di default. Infine, chiamate la funzione "flag.Parse()" per leggere e memorizzare gli argomenti forniti dall'utente.

```Go
import "flag"

func main() {
    // Creazione di un flag stringa con valore di default "Hello"
    nome := flag.String("nome", "Hello", "Inserisci il tuo nome")

    // Creazione di un flag booleano con valore di default false
    saluto := flag.Bool("saluto", false, "Inserisci true per mostrare un saluto")

    flag.Parse()

    // Utilizzo dei flag
    if *saluto {
        fmt.Println("Ciao", *nome)
    } else {
        fmt.Println(*nome)
    }
}
```

Dopo aver compilato ed eseguito il programma, potrete inserire gli argomenti della riga di comando seguiti dal loro valore, ad esempio: "go run main.go -nome=Giuseppe -saluto=true". In questo caso, l'output del programma sarebbe "Ciao Giuseppe".

## Approfondimento 

Oltre a leggere gli argomenti della riga di comando, il pacchetto "flag" offre anche altre funzionalità utili come la possibilità di definire dei flag obbligatori o di fornire una descrizione per ciascun flag. Inoltre, è possibile utilizzare la funzione "flag.Args()" per leggere gli eventuali argomenti non associati a dei flag.

Per ulteriori informazioni sulle opzioni disponibili con il pacchetto "flag", consultate la documentazione ufficiale di Go.

## Vedi anche

- [Documentazione ufficiale di Go per il pacchetto "flag"](https://golang.org/pkg/flag/)
- [Altro articolo in italiano su come leggere gli argomenti della riga di comando in Go](https://velog.io/@francescol/parsing-della-riga-di-comando-con-golang)