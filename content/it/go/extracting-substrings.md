---
title:    "Go: Estrazione di sottostringhe"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'operazione molto utile nella programmazione in Go. Può essere utilizzata per ottenere parti specifiche di una stringa o svolgere operazioni di manipolazione sui dati.

## Come fare

Per estrarre una sottostringa, è possibile utilizzare il metodo `Substring` della libreria `strings`. Ad esempio, se si vuole estrarre la terza e la quarta lettera di una stringa, si può utilizzare il seguente codice:

```Go
s := "Ciao!"
fmt.Println(s[2:4]) // output: ao
```

Il primo numero indica l'indice del carattere da cui iniziare l'estrazione, mentre il secondo indica l'indice del carattere successivo a quello finale. È anche possibile specificare solo il primo valore, in quel caso verranno estratti tutti i caratteri a partire dall'indice specificato fino alla fine della stringa.

```Go
s := "Buongiorno"
fmt.Println(s[4:]) // output: giorno
```

## Approfondimento

L'estrazione di sottostringhe può anche essere utilizzata per ottenere parti specifiche di una stringa in base a determinati criteri, come ad esempio tramite la funzione `Index` della libreria `strings`. Questa funzione restituisce l'indice del primo carattere corrispondente alla sottostringa specificata.

Ad esempio, se vogliamo ottenere il cognome da una stringa contenente anche il nome, possiamo utilizzare il seguente codice:

```Go
s := "Mario Rossi"
fmt.Println(s[strings.Index(s, " ") + 1:]) // output: Rossi
```

In questo caso, viene utilizzato l'indice della prima occorrenza di uno spazio all'interno della stringa e viene estratto tutto ciò che viene dopo.

## Vedi anche

- Documentazione ufficiale sulla libreria `strings` in Go: https://golang.org/pkg/strings/
- L'estrazione di sottostringhe in altri linguaggi di programmazione: https://www.geeksforgeeks.org/substring-in-golang
- Esempi pratici di utilizzo delle sottostringhe in Go: https://golangdocs.com/string-functions-in-golang#9_Extract_a_Substring