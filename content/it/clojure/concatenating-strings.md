---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cosa e perché? 
La concatenazione delle stringhe consiste nell'unire due o più stringhe per formarne una sola. I programmatori lo fanno per creare e manipolare dinamicamente informazioni testuali, facilitando la lettura e la padronanza dei loro codici.

## Come fare:
Clojure offre diversi modi per concatenare le stringhe. Due metodi comunemente utilizzati sono `str` e `format`.

```Clojure
;; Metodo str
(def str1 "Ciao")
(def str2 ", mondo!")
(println (str str1 str2))  ;; Output: Ciao, mondo!

;; Metodo format
(def name "Giovanni")
(println (format "Ciao, %s" name))  ;; Output: Ciao, Giovanni
```

## Approfondimenti:
La concatenazione delle stringhe ha una lunga storia nei linguaggi di programmazione e Clojure non fa eccezione. 

1. Contesto storico - Prima dell'introduzione di funzioni come `str` e `format`, la concatenazione delle stringhe era un processo più complesso e meno efficiente.

2. Alternative - Se sei interessato a saperne di più sulle funzioni di concatenazione di stringhe meno comuni, dai un'occhiata a `join` e `interpose`.

```Clojure
(def names ["Marco" "Anna" "Luca"])
(clojure.string/join ", " names)  ;; Output: "Marco, Anna, Luca"

(def vars ["one" "two" "three"])
(clojure.string/interpose ", " vars)  ;; Output: ("one" ", " "two" ", " "three")
```

3. Dettagli di implementazione - Le funzioni `str` e `format` in Clojure sono ampiamente ottimizzate. `str` è preferibile quando si uniscono poche stringhe, mentre `format` è utile quando si vuole un controllo maggiore sulla formattazione della stringa.

## Vedi anche:
Se vuoi approfondire ancora di più, questi sono alcuni link utili:
- Documentazione ufficiale di Clojure per gli sviluppatori: https://clojure.org/guides/learn/syntax#_strings
- Esercizi di concatenazione di stringhe in Clojure: https://exercism.io/tracks/clojure/exercises/string-concatenation
- Tutorial di concatenazione di stringhe in Clojure su ClojureDocs: https://clojuredocs.org/clojure.core/str