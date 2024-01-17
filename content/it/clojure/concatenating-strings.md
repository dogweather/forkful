---
title:                "Unirsi le stringhe"
html_title:           "Clojure: Unirsi le stringhe"
simple_title:         "Unirsi le stringhe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Concatenare le stringhe in programmazione significa unire più stringhe in una sola. I programmatori spesso lo fanno per creare stringhe più lunghe da stringhe più corte, o per costruire messaggi di errore personalizzati.

## Come:
```
Clojure
;; Per concatenare le stringhe, puoi usare la funzione "str". Ecco un esempio:
(str "Ciao" "a" "tutti") 
;; output => "Ciaoa tutti"
```

## Approfondimento:
La concatenazione delle stringhe è una tecnica comune in programmazione, ma non è l'unico modo per unire le stringhe. Alcuni linguaggi di programmazione usano l'operatore "+" per concatenare le stringhe, mentre in altri si possono utilizzare funzioni specifiche come "concat" in Clojure. Inoltre, la concatenazione delle stringhe può essere un'operazione costosa in termini di prestazioni, poiché viene creato un nuovo oggetto ogni volta che si concatenano le stringhe. In questi casi, è consigliabile utilizzare la classe "StringBuilder" per migliorare le prestazioni.

## Guarda anche:
Documentazione ufficiale di Clojure sulla funzione "str": https://clojuredocs.org/clojure.core/str
Un articolo di riferimento sulle varie opzioni per la concatenazione delle stringhe in Java: https://www.baeldung.com/java-string-concatenation