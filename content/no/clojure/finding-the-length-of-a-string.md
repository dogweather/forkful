---
title:                "Å finne lengden av en streng"
html_title:           "Clojure: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng, eller en tekststreng, er en vanlig utfordring i programmering. Dette kan være nyttig for å manipulere tekst, søke etter spesifikke deler av en streng eller bare for å få informasjon om dataene du arbeider med.

## Hvordan gjøre det

For å finne lengden til en streng i Clojure, kan du bruke funksjonen `count`. Her er et eksempel på hvordan du kan bruke den:

```Clojure
(count "Hei, verden!")  ; gir ut 13
```

Som du kan se, gir denne koden ut lengden til strengen "Hei, verden!" som er 13 tegn.

Du kan også bruke `count` på lister, vektorer og andre samlinger i Clojure. Her er et annet eksempel:

```Clojure
(count [1 2 3 4])  ; gir ut 4
```

Det er viktig å merke seg at `count` returnerer et heltall, så hvis du trenger en mer nøyaktig lengde som inkluderer spesielle tegn, kan du bruke `count` på en konvertert streng, som vist i dette eksemplet:

```Clojure
(count (str "Hei, verden!" :encoding "UTF-8"))  ;gir ut 12
```

## Dykk dypere

Hvis du er interessert i å forstå hvordan `count` fungerer i Clojure, kan du ta en titt på kildekoden for funksjonen. Den er definert som følger:

```Clojure
(defn count
  "Returns the number of items in the collection. (count nil) returns
  0."
  {:added "1.0"
   :static true}
  (^long [coll] (. clojure.lang.RT (count coll))))
```

Som du kan se, bruker `count` funksjonen `clojure.lang.RT/count` til å finne lengden på samlingen du gir den.

## Se også

[Offisiell Clojure Dokumentasjon](https://clojure.org/guides/getting_started)

[Clojure Cookbook](https://github.com/clojure-cookbook/clojure-cookbook)

[Clojure for the Brave and True](https://www.braveclojure.com/)