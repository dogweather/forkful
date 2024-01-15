---
title:                "Estrazione di sottostringhe"
html_title:           "Go: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario e utile estrarre porzioni specifiche di una stringa di testo. Ad esempio, può essere utile per manipolare dati o creare pattern di ricerca. Con Go, è possibile facilmente e in modo efficiente estrarre substrati grazie alle sue potenti funzioni di manipolazione delle stringhe.

## Come fare

Per iniziare, assicurati di avere l'ultima versione di Go installata sul tuo computer. Quindi, segui questi semplici passaggi per estrarre un sottostringa da una stringa:

1. Dichiarare la stringa originale: ```Go
   str := "Questo è un esempio di stringa"
   ```
2. Utilizzare la funzione ```Go
   substring := str[10:17] // estrae "esempio"
   ```
3. Stampare la sottostringa estratta: ```Go
   fmt.Println(substring) // output: "esempio"
   ```
È anche possibile utilizzare la funzione di Go ```strings.SubString()``` per estrarre sottostringhe basate su un determinato delimitatore o carattere di fine.

## Approfondimento

Oltre alla funzione ```substring()```, Go offre una vasta gamma di funzioni di manipolazione delle stringhe per estrarre specifiche porzioni di testo. Ad esempio, la funzione ```strings.Index()``` può essere utilizzata per trovare l'indice di una determinata sottostringa all'interno di una stringa. Inoltre, Go supporta anche l'utilizzo di espressioni regolari per estrarre substrati basati su un modello specifico.

Inoltre, quando si lavora con grandi quantità di dati, Go è molto efficiente grazie alla sua gestione dei buffer e alla sua veloce gestione delle stringhe. Ciò significa che è perfetto per l'estrazione di substrati da stringhe di grandi dimensioni.

## Vedi anche

- [Documentazione ufficiale di Go per la manipolazione delle stringhe](https://golang.org/pkg/strings/)
- [Guida completa alla gestione delle stringhe in Go](https://www.digitalocean.com/community/tutorials/how-to-use-strings-in-go)
- [Blog di Go sull'utilizzo dell'espressione regolare in Go](https://blog.golang.org/regexp)