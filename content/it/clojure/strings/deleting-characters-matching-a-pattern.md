---
date: 2024-01-20 17:41:51.982600-07:00
description: 'How to: In Clojure, usiamo `clojure.string/replace` per sostituire i
  caratteri indesiderati. Ecco come.'
lastmod: '2024-03-13T22:44:43.027166-06:00'
model: gpt-4-1106-preview
summary: In Clojure, usiamo `clojure.string/replace` per sostituire i caratteri indesiderati.
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## How to:
In Clojure, usiamo `clojure.string/replace` per sostituire i caratteri indesiderati. Ecco come:

```Clojure
(require '[clojure.string :as str])

; Elimina tutti i numeri da una stringa
(defn delete-digits [s]
  (str/replace s #"\d+" ""))

; Uso della funzione
(println (delete-digits "Terza1Class3"))
```

Output:
```
TerzaClass
```

Un altro esempio per rimuovere le vocali:

```Clojure
; Elimina tutte le vocali da una stringa
(defn delete-vowels [s]
  (str/replace s #"[aeiouAEIOU]" ""))

; Uso della funzione
(println (delete-vowels "Ciao Mondo"))
```

Output:
```
C Mnd
```

## Deep Dive
La funzione `clojure.string/replace` esiste da molte versioni. È basata su Java `String.replaceAll`, che usa espressioni regolari. In alternativa, puoi usare `reduce` con una sequenza di caratteri o altre funzioni core di Clojure.

Per esempio, con `reduce`:

```Clojure
(defn delete-chars [s chars-pattern]
  (reduce (fn [acc char]
            (str/replace acc (str char) ""))
          s
          chars-pattern))

(println (delete-chars "Hello World" "lo"))
```

Output:
```
He Wrd
```

Dettagli di implementazione: `str/replace` è veloce e potente grazie alle regex. C'è da fare attenzione con pattern complessi per evitare un uso eccessivo di risorse.

## See Also
- ClojureDocs su `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Guida alle regex in Java (usate anche in Clojure): https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- Clojure from the ground up: strings: https://aphyr.com/posts/305-clojure-from-the-ground-up-strings
