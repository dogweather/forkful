---
title:                "Estrarre le sottostringhe"
html_title:           "Go: Estrarre le sottostringhe"
simple_title:         "Estrarre le sottostringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Estrarre sottostringhe è un'operazione comune nella programmazione, in cui si ottiene una porzione più piccola di una stringa più grande. Gli sviluppatori utilizzano questa funzione per una varietà di scopi, come la manipolazione dei dati, la validazione delle informazioni o la creazione di nuove stringhe basate su quelle esistenti.

## Come fare:

Estrarre sottostringhe in Go è semplice e diretto. Ecco alcuni esempi di codice che mostrano come farlo utilizzando la funzione "substring", che prende come argomenti la stringa di origine, l'indice di inizio e la lunghezza della sottostringa desiderata.

```
Go code:

//Esempio: estrazione di una sottostringa da una stringa
line := "Questo è un esempio di stringa"
extract := line[11:18]
fmt.Println(extract)

//Output: un esempio
``` 

```
Go code:

//Esempio: estrazione di una singola lettera da una stringa
word := "ciao"
extract := word[2]
fmt.Println(extract)

//Output: i
```

```
Go code:

//Esempio: estrazione di una sottostringa fino alla fine della stringa
sentence := "Vieni a trovarmi al parco domani!"
extract := sentence[17:]
fmt.Println(extract)

//Output: parco domani!
```

## Approfondimento:

Estrarre sottostringhe ha una lunga storia nella programmazione, in quanto è un'operazione comune ed essenziale. Tuttavia, esistono anche altre funzioni in Go che permettono di manipolare le stringhe, come "strings.Index" per trovare la posizione di una sottostringa all'interno di una stringa più grande, o "strings.Replace" per sostituire una sottostringa con un'altra.

L'implementazione di Go per l'estrazione di sottostringhe è efficiente e robusta, utilizzando un approccio basato sull'indice dei caratteri all'interno della stringa di origine. Ci sono anche alcune librerie di terze parti che offrono funzionalità aggiuntive per l'estrazione di sottostringhe, come "stringutil" o "strutil".

## Vedi anche:

Ecco alcuni link utili per saperne di più sull'estrazione di sottostringhe in Go:

- Documentazione ufficiale Go: https://golang.org/pkg/strings/#Substring
- Tutorial sui fondamenti delle stringhe in Go: https://www.tutorialspoint.com/go/go_strings.htm
- Discussione sulla manipolazione delle stringhe su Golang Bridge: https://golangbridge.org/post/shorter-substring-in-go/