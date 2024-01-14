---
title:                "Clojure: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har jobbet med websider, har du sikkert hørt om HTML, språket som brukes til å strukturere og formatere nettsider. Men har du noen gang tenkt på å parse, eller analysere, denne koden med Clojure? Ved å parse HTML kan du hente ut spesifikk informasjon fra en webside og bruke den til å utføre forskjellige oppgaver, som å generere rapporter eller automatisere datainnsamling. Les videre for å lære mer om hvordan du kan gjøre dette.

## Hvordan

For å parse HTML i Clojure, trenger du biblioteket `clojure.xml` som er innebygd i standardbiblioteket. La oss si at vi vil hente ut alle lenker på en side og lagre dem i en liste. Vi kan bruke funksjonen `parse` for å konvertere HTML-koden til et Clojure-datastrukturobjekt, som vi deretter kan navigere gjennom og hente ut de elementene vi er interessert i.

```Clojure
(ns html-parser.core
  (:require [clojure.xml :as xml]
            [clojure.string :as string]))

;; Henter HTML-koden fra en nettadresse
(def url "https://www.example.com")

;; Bruker parse-funksjonen til å konvertere HTML-koden til et objekt
(def html (xml/parse url))

;; Velger ut elementet "a" som representerer lenker
(def links (xml/select html [:a]))

;; Bruker map-funksjonen til å hente ut verdiene fra attributtet "href" fra alle lenkene
(def link-values (map #(:href %) links))

;; Skriver ut den nye listen av lenker
(string/join "\n" link-values)
```

Denne koden vil returnere en liste av alle lenkene på nettsiden. Resultatet vil se slik ut:

```
/about
/contact
/services
```

## Dykk dypere

HTML er et hierarkisk språk, med tags som utgjør forskjellige nivåer av elementer. Dette betyr at du også kan navigere gjennom og hente ut informasjon fra indre elementer. For eksempel kan du bruke funksjonen `:content` for å hente ut innholdet inni en tag, eller `:attrs` for å hente ut verdiene i attributtene til en tag. Ved å kombinere forskjellige funksjoner, kan du utforske og hente ut ulike elementer fra en webside.

## Se også

- `clojure.xml` dokumentasjon: https://clojuredocs.org/clojure.xml
- Offisiell Clojure tutorials: https://clojure.org/guides/getting_started