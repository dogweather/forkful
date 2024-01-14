---
title:                "Go: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
L'estrazione di sottostringhe è un'operazione comune e potente in programmazione, che consente di manipolare le stringhe in modo più preciso e efficiente. In questo post, scopriremo come utilizzare questa funzione nel linguaggio di programmazione Go.

## Come fare
Per iniziare ad estrarre sottostringhe in Go, è necessario utilizzare la funzione `Substring` della libreria `strings`. Questa funzione richiede tre argomenti: la stringa di origine da cui estrarre la sottostringa, l'indice di inizio e l'indice di fine della sottostringa desiderata.

```Go
import "strings"

// Esempio di utilizzo della funzione Substring per estrarre una sottostringa
fmt.Println(strings.Substring("Buongiorno", 4, 8)) 
// Output: giorn
```

Come mostra l'esempio, la funzione `Substring` estrae la porzione di stringa tra l'indice di inizio (incluso) e l'indice di fine (escluso). È importante notare che gli indici in Go partono da 0.

È possibile anche utilizzare l'operatore colons (`:`) per estrarre la sottostringa senza specificare l'indice di fine. Ad esempio:

```Go
fmt.Println(strings.Substring("Buongiorno", 4, -1))
// Output: giorn
```

In questo caso, l'operatore colons indica di estrarre la sottostringa dalla posizione specificata fino alla fine della stringa.

## Approfondimento
L'operazione di estrazione di sottostringhe può essere applicata in diversi casi, ad esempio per:
- Ottenere una parte specifica di una stringa, come il cognome in un elenco di nomi completi
- Rimuovere o sostituire una porzione di una stringa, come un carattere speciale
- Verificare se una sottostringa è contenuta all'interno di una stringa più grande

Per maggiori informazioni sull'utilizzo della funzione `Substring` e sulle diverse opzioni disponibili, ti consiglio di consultare la documentazione ufficiale di Go.

## Vedi anche
- [Documentazione ufficiale di Go](https://golang.org/pkg/strings/#Substring)
- [Guida all'utilizzo delle stringhe in Go](https://www.digitalocean.com/community/tutorials/how-to-use-strings-in-go)
- [Esempi di utilizzo della funzione Substring](https://golangbyexample.com/golang-substring-from-string/)