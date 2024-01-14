---
title:    "Clojure: Sletting av tegn som matcher et mønster"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster er en vanlig oppgave i programmering. Det kan hjelpe deg med å forenkle og rydde opp i dataene dine, og gjøre dem mer leselige og effektive å arbeide med. I Clojure er det flere måter å håndtere dette på, avhengig av dine spesifikke behov og preferanser.

## Hvordan

En av de mest brukte funksjonene for å slette tegn som matcher et mønster er `re-seq`. Denne funksjonen tar et regulært uttrykk og en streng som argumenter, og returnerer en liste med delstrenger som matcher mønsteret.

```Clojure 
(re-seq #"[aeiou]" "Hei på deg!")
;;=> ("e" "i" "å" "e")

(re-seq #"\d+" "I dag er det 17. mai.")
;;=> ("17")

(re-seq #"[a-z]+" "Han er anonym123.")
;;=> ("anonym")
```

Dette er nyttig hvis du bare ønsker å slette spesifikke typer tegn, som vokaler, tall eller bokstaver. Men hva om du vil slette mer komplekse mønstre? En annen tilnærming er å bruke `clojure.string/replace-first` og `clojure.string/replace`. Disse funksjonene tar henholdsvis et mønster og en erstatning, og en streng som argumenter, og returnerer en ny streng med det første, eller alle, forekomster av mønsteret erstattet med erstatningen.

```Clojure 
(require '[clojure.string :as str])
(str/replace-first "Hvem er kongen av feltet?" #"feltet" "tronen")
;;=> "Hvem er kongen av tronen?"

(str/replace "Peter Parker" #"P[a-z]+" "")
;;=> " "

(str/replace "0000-1111-2222-3333" #"\d" "X" 4)
;;=> "XXXX-1111-2222-3333"
```

## Dypdykk

Hvis du trenger mer avansert funksjonalitet, kan du også bruke `clojure.string/replace-first` og `clojure.string/replace` sammen med `clojure.string/replace-matcher`. Dette vil tillate deg å tilpasse den nye strengen basert på et `re-matcher` objekt som returneres av `replace-matcher`, noe som gir deg enda mer kontroll over prosessen.

```Clojure 
(require '[clojure.string :as str])
(defn transform-matcher [matcher]
  (let [current (apply str (str/replace-matcher matcher))
        next (str/replace current #"mølle" "foss")]
    (clojure.string/replace-first next #"verden" "jorden")))

(str/replace "Jeg elsker å se på møllen i verden." #"\w+" transform-matcher)
;;=> "Jeg elsker å se på fossen i jorden."
```

## Se også

- Offisiell dokumentasjon for `re-seq`: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/re-seq
- Offisiell dokumentasjon for `clojure.string/replace-first` og `clojure.string/replace`: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace-first
- Offisiell dokumentasjon for `clojure.string/replace-matcher`: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace-matcher