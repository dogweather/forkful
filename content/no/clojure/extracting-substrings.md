---
title:                "Uthenting av delstrenger"
date:                  2024-01-20T17:45:26.226975-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Substrenger lar oss hente spesifiserte deler av en streng. Nyttig for å analysere tekst, hente ut data, eller formatere utdata.

## How to:
Ekstrahering av substringer i Clojure er rett fram. Bruk `subs` funksjonen. Her er et eksempel:

```clojure
(let [tekst "Hei, Norge!"]
  (subs tekst 0 3)) ; => "Hei"

(let [tekst "Clojure er kul"]
  (subs tekst 8))   ; => "kul"
```

`subs` tar teksten, start- og sluttindeks. Fjern sluttindeks for å ta resten av strengen.

## Deep Dive
Substrenger er ikke nytt. De eksisterte i Lisp, Clojure's stamfar. Clojure, en moderne Lisp-dialekt, håndterer strenger som i Java, siden det kjører på JVM (Java Virtual Machine).

Alternativer? Regulære uttrykk (regex) til komplekse søk, `split` for å dele en streng ved et mønster, eller string biblioteker for mer kraft.

Implementasjon? `subs` er en wrapper rundt Java's `substring`, rask og effektiv. Men, vær nøye! Feil indekser gir `StringIndexOutOfBoundsException`.

## See Also
- ClojureDocs (`subs` funksjon): [https://clojuredocs.org/clojure.core/subs](https://clojuredocs.org/clojure.core/subs)
- JavaDoc (forståelse av `substring`): [https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
- Clojure from the ground up (string manipulasjon): [https://aphyr.com/posts/305-clojure-from-the-ground-up-strings](https://aphyr.com/posts/305-clojure-from-the-ground-up-strings)
