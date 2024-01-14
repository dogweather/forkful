---
title:    "Clojure: Sammenslåing av strenger"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere eller "kata" strenger spiller en viktig rolle i mange programmeringsoppgaver. Ved å gjøre dette kan du enkelt konstruere store tekststrenger fra mindre biter som bidrar til å gjøre koden din mer lesbar og lett å vedlikeholde.

## Hvordan

Du kan kombinere strenger ved å bruke funksjonen "str" i Clojure. Denne funksjonen tar imot en uendelig mengde argumenter, og returnerer en enkeltstreng som er en kombinasjon av disse.

```Clojure
(str "Hei" " " "alle sammen") ; Output: "Hei alle sammen"
(str "abc" "def" "ghi") ; Output: "abcdefghi"
```

Du kan også kombinere strenger ved å bruke "+" operatoren.

```Clojure
(+ "Hva er" " " "opp?") ; Output: "Hva er opp?"
```

Hvis du vil kombinere en streng med et tall eller et annet data type, kan du bruke funksjonen "format". Denne funksjonen tar imot en formatteringsstreng og argumenter som skal plasseres i denne strengen.

```Clojure
(format "Det var %s ganger jeg sa hei i dag." 5) ; Output: "Det var 5 ganger jeg sa hei i dag."
(format "Jeg vant %d kroner i lotto!" 1000000) ; Output: "Jeg vant 1000000 kroner i lotto!"
```

## Dypdykk

Det er viktig å være oppmerksom på at string-konkatinering kan bli ineffektivt hvis du bruker det i løkker eller funksjoner som kalles mange ganger. Dette er fordi hver gang du kombinerer to strenger, blir en kopi av dem laget, som kan føre til at programmet ditt bruker mer minne enn nødvendig.

For å unngå dette kan du bruke "StringBuilder" klassen i Java interop. Denne klassen lar deg bygge en streng ved å legge til små biter for å unngå å måtte skape mange kopier.

```Clojure
(require '[clojure.string :as str])
(defn concatenate [list-of-strings]
  (let [builder (java.lang.StringBuilder.)]
    (doseq [str list-of-strings]
      (.append builder str))
    (.toString builder)))

(concatenate ["Hei, " "jeg " "heter " "Per"]) ; Output: "Hei, jeg heter Per"
```

## Se også 

- [Clojure Docs - String Concatenation](https://clojuredocs.org/clojure.core/str)
- [Java StringBuilder Class](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Clojure for Begynnere - Append og Str Concatenation](http://clojure-begginers-guide.blogspot.com/2016/07/append-and-str-concatenation.html)