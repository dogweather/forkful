---
title:    "Clojure: Søke og erstatte tekst"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Det er viktig å kunne søke og erstatte tekst når man jobber med programmering, spesielt i Clojure hvor funksjonell programmering er viktig. Dette gjør det enkelt å gjøre flere endringer samtidig og for å redusere manuelt arbeid.

# Hvordan

For å søke og erstatte tekst i Clojure, kan man bruke funksjonen "replace" og "re-pattern". Her er et enkelt eksempel på hvordan man kan erstatte alle forekomster av "he" med "she" i en liste av strenger:

```Clojure
(def strenger ["Han heter John" "Hun heter Jill" "Hei alle"])

(map #(clojure.string/replace % #"(?i)he" "she") strenger)

;; Output: ("She heter John" "Hun heter Jill" "Shei alle")
```

Her bruker vi "#"(?i)he"" for å ignorere store og små bokstaver. Vi kan også bruke regulære uttrykk for mer komplekse søk og erstattinger.

# Dypdykk

I Clojure kan man også bruke rekursive funksjoner for å gjøre søk og erstatting på dypere nivåer i datastrukturer. For eksempel, hvis vi har en liste med lister av tall, og vi ønsker å erstatte alle positive tall med deres negative motstykke, kan vi bruke en rekursiv funksjon slik:

```Clojure
(def tall [[1 -2 3] [4 -5 6]])

(defn negativ-replace [x]
  (if (list? x)
      (map negativ-replace x)
      (if (> x 0) (- x) x)))

(negativ-replace tall)

;; Output: [[-1 2 -3] [-4 5 -6]]
```

Her bruker vi funksjonen "negativ-replace" på hver enkelt verdi i strukturen og bruker "map" for å mappe funksjonen over alle elementene.

# Se Også

- [Clojure API Dokumentasjon for replace](https://clojuredocs.org/clojure.core/replace)
- [Clojure API Dokumentasjon for re-pattern](https://clojuredocs.org/clojure.core/re-pattern)
- [RegExr - regex tester og viser forklaringer på regulære uttrykk](https://regexr.com/)