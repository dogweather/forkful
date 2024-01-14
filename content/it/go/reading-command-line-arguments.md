---
title:    "Go: Lettura degli argomenti della riga di comando"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

Leggere gli argomenti della riga di comando può essere utile quando si vuole passare delle opzioni personalizzate al proprio programma Go. Ad esempio, è possibile impostare delle variabili globali o specificare un file di input.

## Come Fare

Per leggere gli argomenti della riga di comando in Go, è sufficiente utilizzare la funzione `os.Args`, che restituisce una slice contenente tutti gli argomenti passati al programma. Ecco un esempio di codice:

```Go
// Legge gli argomenti della riga di comando
args := os.Args

// Stampa il numero totale di argomenti passati
fmt.Println(len(args))

// Stampa tutti gli argomenti (escluso il nome del programma)
for i := 1; i < len(args); i++ {
  fmt.Println(args[i])
}
```

Output:

```
3
opzione1
opzione2
```

## Approfondimento

La funzione `os.Args` restituisce una slice di tipo `[]string`, ma è possibile utilizzare il package `flag` per gestire in modo più avanzato gli argomenti della riga di comando. Ad esempio, è possibile definire delle opzioni o dei flag che verranno impostati dall'utente in fase di esecuzione del programma.

Per maggiori informazioni sul package `flag` e su come utilizzarlo, si consiglia di consultare la [documentazione ufficiale di Go](https://golang.org/pkg/flag/) o di cercare tutorial online.

## Vedi Anche

- [Documentazione ufficiale di Go su os.Args](https://golang.org/pkg/os/#Args)
- [Documentazione ufficiale di Go su flag](https://golang.org/pkg/flag/)
- [Tutorial su come leggere argomenti della riga di comando in Go](https://www.digitalocean.com/community/tutorials/how-to-handle-command-line-arguments-in-go)