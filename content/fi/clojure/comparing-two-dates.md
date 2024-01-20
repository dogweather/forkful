---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi? 
Päivämäärien vertaaminen tarkoittaa kahta ajankohtaa suhteessa toisiinsa. Ohjelmoijat tekevät tämän selvittääkseen esimerkiksi, onko yksi päivämäärä ennen toista tai kuinka kauan kahden päivämäärän välinen ero on.

## Miten:
`java.time.LocalDate` ja `java.time.Period` -luokkia voidaan käyttää päivämäärien vertaamiseen Clojure-ohjelmassa.

```clojure
(ns comparing-dates
  (:import [java.time LocalDate Period]))

(def date1 (LocalDate/of 2020 1 1))
(def date2 (LocalDate/of 2021 1 1))

(def period (Period/between date1 date2))
  
(println (.getYears period)) ;; tulostaa 1
```

## Syvällisemmin:
Päivämäärien vertaaminen ei ole uusi käsite, vaan se on peräisin ohjelmoinnin alkuajoista. Useita tapoja on esitetty ajankohdan määrittämiseen; joitakin näistä tavoista käytetään edelleen.

`java.time`-kirjasto, joka esiteltiin Javassa 8, tarjoaa useita luokkia, kuten `LocalDate` ja `Period`, joita voidaan käyttää päivämäärien vertailuun.

On olemassa myös muita kirjastoja, kuten Joda-Time, jotka tarjoavat samanlaisia ominaisuuksia. Mutta ne ovat nykyään vähemmän suosittuja `java.time`-kirjaston käyttöönoton jälkeen.

Yksityiskohdat päivämäärien vertailusta ja niiden edustuksesta voivat vaihdella ohjelmointikielillä. Joissakin niistä, kuten Pythonissa, päivämäärän tyyppi on sisäänrakennettu, mutta toisissa, kuten JavaScriptissä, se ei ole.

## Katso myös:
- Java-ohjelman päivämäärien käsittely ja vertaaminen: https://docs.oracle.com/javase/tutorial/datetime/
- Päivämäärien vertailu Joda-Time-kirjastossa: https://www.joda.org/joda-time/
- Päivämäärien vertaaminen eri ohjelmointikielissä: https://www.codeproject.com/Articles/2750/Handling-Dates-and-Time-in-Various-Programming-Lan