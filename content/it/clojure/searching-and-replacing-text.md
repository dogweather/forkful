---
title:                "Cerca e sostituisci testo"
html_title:           "Clojure: Cerca e sostituisci testo"
simple_title:         "Cerca e sostituisci testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con il tuo codice Clojure e ti ritrovi a dover sostituire un pezzo di testo con un altro, allora questo articolo è per te. Con il giusto approccio, puoi risparmiare tempo e rendere il tuo lavoro più efficiente.

## Come Fare

Per sostituire un pezzo di testo con un altro in Clojure, puoi utilizzare la funzione `replace` della libreria `clojure.string`. Ad esempio:

```
Clojure (clojure.string/replace "Ciao, mondo!" "Ciao" "Hello")
```

Output: "Hello, mondo!"

In questo esempio, "Ciao" viene sostituito con "Hello". Puoi anche utilizzare la funzione `replace-first` per sostituire solo la prima occorrenza di un testo, invece di tutte.

```
Clojure (clojure.string/replace-first "Ciao, mondo! Ciao" "Ciao" "Hello")
```

Output: "Hello, mondo! Ciao"

Puoi anche utilizzare delle espressioni regolari nella funzione `replace` per cercare testi più complessi da sostituire:

```
Clojure (clojure.string/replace "Il mio numero di telefono è 123-456-789" #"\d+-\d+-\d+" "XXX-XXX-XXXX")
```

Output: "Il mio numero di telefono è XXX-XXX-XXXX"

## Approfondimento

Se vuoi approfondire le tue conoscenze sulle espressioni regolari e su come utilizzarle per sostituire testi in modo più avanzato, puoi consultare questi link:

- Documentazione ufficiale sulla funzione `replace`: https://clojuredocs.org/clojure.string/replace
- Tutorial sulle espressioni regolari in Clojure: https://clojure.org/guides/learn/regular_expressions

## Vedi Anche

- Documentazione ufficiale sulla libreria `clojure.string`: https://clojuredocs.org/clojure.string
- Tutorial Clojure sul sito ufficiale: https://clojure.org/guides/getting_started