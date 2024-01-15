---
title:                "Estrazione di sottostringhe"
html_title:           "Clojure: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

C'è sempre una prima volta per tutto, anche per estrarre sottostringhe in Clojure. Se ti trovi a lavorare con testi e vuoi ottenere una sezione specifica di un determinato testo, potresti trovare utile l'estrazione delle sottostringhe. Potrebbe sembrare un concetto complesso, ma con il giusto approccio, puoi farlo in modo semplice ed efficiente.

## Come fare

L'estrazione delle sottostringhe in Clojure è possibile utilizzando la funzione `subs` insieme agli indici di posizione della sottostringa desiderata.

```Clojure
(def testo "Questo è un testo di esempio")

(subs testo 0 5)

;Output: "Questo"
```

In questo esempio, abbiamo utilizzato `subs` per estrarre la sottostringa da 0 a 5 (compreso) del testo. Puoi anche utilizzare numeri negativi per indicare l'inizio e la fine della sottostringa rispetto alla fine del testo.

```Clojure
(subs testo -7 -1)

;Output: "esempio"
```

Puoi anche passare un solo parametro per estrarre la sottostringa dall'inizio o dalla fine del testo.

```Clojure
(subs testo 12)

;Output: "testo di esempio"

(subs testo -7)

;Output: "esempio"
```

## Approfondimento

Oltre alla funzione `subs`, esiste anche la funzione `substring` che accetta tre parametri: il testo di cui estrarre la sottostringa, l'indice di inizio e la lunghezza della sottostringa desiderata.

Inoltre, puoi utilizzare anche espressioni regolari per catturare estrarre sottostringhe basate su un determinato pattern di caratteri.

## Vedi anche

- [Clojure String Manipulation](https://clojure.org/guides/strings)
- [Clojure Substring Function](https://clojuredocs.org/clojure.core/substring)