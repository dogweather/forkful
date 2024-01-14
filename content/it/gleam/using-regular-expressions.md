---
title:                "Gleam: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Perché

L'utilizzo delle espressioni regolari è essenziale per analizzare e manipolare i dati in modo efficiente nei programmi Gleam. Questo strumento permette di trovare pattern specifici all'interno di una stringa di testo, semplificando il processo di ricerca e sostituzione delle informazioni.

##Come utilizzarle

Per utilizzare le espressioni regolari in Gleam, basta usare il modulo `Regex` e la sua funzione `match`. Ad esempio, per cercare una parola specifica in una stringa si può scrivere il seguente codice:

```Gleam
import Regex

let stringa = "Questo è un esempio di stringa"
let parola_cercata = "esempio"

let match = Regex.match(parola_cercata, stringa)

```

Il risultato sarà un `Option` di tipo `Regex.Match`, che contiene informazioni sulla posizione della parola cercata all'interno della stringa di testo. Per ottenere il valore effettivo, si può utilizzare il pattern matching sul `Option`:

```Gleam
let stringa_trovata = match {
    Just(trovato) -> stringa[trovato.offset..trovato.trailing]
    _ -> "Parola non trovata"
}

```

In questo modo, `stringa_trovata` conterrà la stringa "parola" che è stata cercata nella stringa originale.

##Approfondimento

Le espressioni regolari sono potenti, ma richiedono una certa conoscenza dei pattern e delle loro sintassi. Ad esempio, si possono utilizzare i caratteri speciali come `^` per indicare l'inizio della stringa e `$` per indicare la fine. Inoltre, è possibile specificare l'utilizzo di caratteri wildcards per trovare parole con varie forme o varianti, grazie all'utilizzo di simboli come `*` e `+`.

Per una lista completa delle funzionalità delle espressioni regolari in Gleam, si consiglia di consultare la documentazione ufficiale.

##Vedi anche

- [Documentazione ufficiale di Gleam sulle espressioni regolari] (https://gleam.run/book/tour/regex.html)
- [Tutorial su come utilizzare espressioni regolari in Gleam] (https://dev.to/lgg/using-regular-expressions-in-gleam-87k)
- [Video su come utilizzare espressioni regolari in Gleam] (https://www.youtube.com/watch?v=Nn9ufoOE_CE)