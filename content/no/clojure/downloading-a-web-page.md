---
title:                "Clojure: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å laste ned en nettside. En vanlig grunn er at du vil lagre informasjonen på siden for senere bruk, eller for å kunne få tilgang til den offline. Det kan også være at du vil bruke siden som en del av et større program eller prosjekt.

Uansett grunn så er det nyttig å vite hvordan man kan laste ned en nettside ved hjelp av Clojure. Så la oss ta en titt på hvordan det kan gjøres!

## Slik gjør du det

For å laste ned en nettside i Clojure, kan du bruke funksjonen `slurp` fra `clojure.java.io` biblioteket. Denne funksjonen tar imot en URL som parameter og returnerer innholdet på den gitte siden som en streng.

```Clojure
(ns min-prosjekt.core
  (:require [clojure.java.io :refer [slurp]]))

(defn last-ned-side [url]
  (slurp url))
```

For å bruke denne funksjonen, trenger du bare å kalle den med en gyldig URL som parameter og lagre resultatet i en variabel.

```Clojure
(def side-innhold (last-ned-side "https://www.google.com/"))
```

Deretter kan du gjøre hva du vil med innholdet på siden, enten det er å skrive det ut til konsollen, lagre det til en fil eller behandle det videre.

## Dypdykk

Det er også mulig å laste ned en nettside ved hjelp av `clojure.contrib.browse` biblioteket. Denne metoden gir mer kontroll over hvordan du vil håndtere eventuelle feil og håndtere nettverkskommunikasjon.

For å bruke dette biblioteket må du legge det til i din `project.clj` fil som en avhengighet:

```Clojure
:dependencies [[org.clojure/clojure "1.10.0"]
               [org.clojure/contrib "1.3.0"]]
```

Deretter kan du definere en funksjon som bruker `get-page` funksjonen fra `clojure.contrib.browse.surf` for å laste ned en nettside:

```Clojure
(ns min-prosjekt.core
  (:require [clojure.contrib.browse.surf :as surf]))

(defn last-ned-side [url]
  (let [http-res (surf/get-page url)]
    (slurp (:stream http-res))))
```

Her bruker vi `get-page` funksjonen for å få en HTTP respons fra den gitte URL-en, og deretter bruker vi `slurp` funksjonen for å hente innholdet fra response streamen.

## Se også

Her er noen nyttige ressurser for å lære mer om å laste ned nettsider i Clojure:

- [GitHub: clojure.java.io](https://github.com/clojure/java.io)
- [GitHub: clojure.contrib.browse](https://github.com/clojure-contrib/browse)
- [ClojureDocs: slurp](https://clojuredocs.org/clojure.java.io/slurp)
- [ClojureDocs: get-page](https://clojuredocs.org/clojure.contrib.browse.surf/get-page)

Nå har du en god forståelse av hvordan du kan laste ned nettsider ved hjelp av Clojure. Lykke til med nedlasting av alle de interessante siden du vil utforske videre!