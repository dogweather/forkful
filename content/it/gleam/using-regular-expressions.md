---
title:                "Utilizzare le espressioni regolari"
html_title:           "Gleam: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Le espressioni regolari sono uno strumento fondamentale per i programmatori per trovare e manipolare testo in modo efficiente e preciso. Sono utilizzate in molte lingue di programmazione, tra cui Gleam, per cercare corrispondenze tra pattern e stringhe di testo.

## Come fare:
Le espressioni regolari in Gleam possono essere create usando il modulo `regex` della libreria standard. Ad esempio, per trovare una corrispondenza di un numero telefonico in una stringa di testo, possiamo usare il seguente codice:

```Gleam
import regex

let telefono_regex = regex.compile("([0-9]{3})-([0-9]{3})-([0-9]{4})")

let stringa_di_testo = "Il mio numero di telefono è 555-123-4567"
let risultato = stringa_di_testo
  |> regex.find(telefono_regex)
  |> Option.map(fn {region, _} -> region)

// Risultato = Some("555-123-4567")
```

Il codice sopra importa il modulo `regex`, compila un'espressione regolare per il formato di un numero di telefono americano e usa il metodo `find` per cercare una corrispondenza all'interno della stringa di testo. Il risultato è un `Option` contenente la corrispondenza trovata.

## Approfondimento:
Le espressioni regolari sono state inventate negli anni '50 dall'informatico statunitense Stephen Kleene. Oggi sono utilizzate in molte lingue di programmazione e strumenti di ricerca avanzata, come gli editor di testo. In alternativa alle espressioni regolari, ci sono anche altre tecniche di matching come le grammatiche regolari e i parser.

Un'implementazione popolare di espressioni regolari è quella del motore di ricerca `PCRE`, che viene utilizzato in molti linguaggi di programmazione. Nel caso di Gleam, l'implementazione è basata sulla libreria `re2`, che è veloce e sicura da usare in un ambiente di concorrenza come quello di Erlang.

## Vedi anche:
- Documentazione ufficiale di Gleam sul modulo `regex`: https://gleam.run/modules/regex/
- Tutorial su come utilizzare espressioni regolari in altre lingue di programmazione: https://www.regular-expressions.info/tutorial.html