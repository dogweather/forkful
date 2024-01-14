---
title:    "Go: Stampa dell'output di debug"
keywords: ["Go"]
---

{{< edit_this_page >}}

##Perché

Spesso, durante la programmazione, ci troviamo di fronte a problemi o bug che non sono facili da individuare. La stampa in output dei determinati valori può essere di grande aiuto nel processo di debugging, consentendoci di comprendere meglio il funzionamento del nostro codice e individuare eventuali errori.

##Come

Per stampare in output dei valori durante l'esecuzione del codice in Go, possiamo utilizzare la funzione `fmt.Println()`, che stampa i valori passati come argomenti a schermo. Ad esempio, se volessimo stampare la stringa "Ciao mondo!", il codice sarebbe il seguente:

```Go
fmt.Println("Ciao mondo!")
```

Possiamo anche stampare più valori in una sola istruzione, separandoli con una virgola. Inoltre, possiamo utilizzare la funzione `fmt.Printf()` per formattare il nostro output in modo più preciso. Ad esempio, se volessimo stampare il valore di una variabile float con solo due cifre decimali, il codice sarebbe il seguente:

```Go
fmt.Printf("Il valore di x è %.2f", x)
```

##Deep Dive

In Go, abbiamo a disposizione diverse opzioni per stampare in output dei valori, come ad esempio `fmt.Println()`, `fmt.Printf()` e `fmt.Sprintf()`. Inoltre, possiamo utilizzare anche la funzione `log` del pacchetto `log` per registrare messaggi di debug in un file di log. 

Oltre alla semplice stampa dei valori, possiamo anche utilizzare delle etichette o delle note per rendere più comprensibili i nostri messaggi di debug. Ad esempio, possiamo utilizzare `\n` per andare a capo o `\t` per aggiungere una tabulazione.

##See Also

- [Documento ufficiale di Go sulla formattazione dei valori in output](https://golang.org/pkg/fmt/)
- [Tutorial su come utilizzare la funzione `log` di Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-logging-package-in-go)
- [Articolo su come utilizzare correttamente la stampa in output durante il debugging](https://medium.com/@zach_4342/debugging-in-go-best-practices-and-common-errors-e6c44f2dcef3)