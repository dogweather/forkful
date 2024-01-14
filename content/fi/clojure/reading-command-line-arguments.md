---
title:                "Clojure: Painettaessa komentoriviparametreja"
simple_title:         "Painettaessa komentoriviparametreja"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentoriviparametreja?

Komentoriviparametrit ovat tärkeä osa ohjelmointia, sillä ne mahdollistavat käyttäjien antaman syötteen käsittelemisen ja ohjelman suorituksen mukauttamisen sen avulla. Komentoriviparametrien lukeminen on erittäin tärkeä taito, joka auttaa tekemään ohjelmista monipuolisempia ja joustavampia.

## Näin luet komentoriviparametreja Clojurella

Komentoriviparametrien lukeminen Clojurella on helppoa ja suoraviivaista. Käytämme tähän tarkoitukseen "command-line" kirjastoa, joka tarjoaa valmiit työkalut komentoriviparametrien käsittelyyn.

```Clojure
(require '[clojure.tools.cli :refer [cli]])
```
Tämän jälkeen voimme asettaa haluamamme parametrit käyttöömme yksinkertaisella komennolla:

```Clojure
(def parameters (cli args
      ["-n" "--name" "User's name" :parse-fn #(.toUpperCase %)]
      ["-a" "--age" "User's age"])
```
Parametrit tallentuvat "parameters" muuttujaan ja niitä voidaan käsitellä helposti koodissa. Esimerkiksi voimme tulostaa käyttäjän nimen ja iän seuraavasti:

```Clojure
(println "Hello" (:name parameters), "you are" (:age parameters), "years old!")
```
Syötteenä annetut parametrit voidaan myös tarkistaa ja validoida ennen niiden käyttöä. Esimerkiksi voimme tarkistaa, että käyttäjä antaa nimen parametrina ennen sen käyttämistä:

```Clojure
(if (:name parameters)
    (println "Hello" (:name parameters))
    (println "Please provide a name with the -n or --name parameter."))
```

## Syvällisempi sukellus komentoriviparametreihin

Komentoriviparametrit voivat olla myös valinnaisia, jolloin niiden antaminen on käyttäjälle vapaaehtoista. Tähän tarkoitukseen voimme käyttää "cli" kirjaston "optional" funktiota, joka sallii parametrien ohittamisen ilman virheilmoituksia.

Komentoriviparametrejä voidaan myös käsitellä monipuolisesti erilaisten funktioiden avulla, kuten "parse-fn" joka mahdollistaa parametrien muokkaamisen ennen niiden tallentamista. Lisäksi komentoriviparametrille voidaan antaa oletusarvo, mikäli käyttäjä ei anna sille mitään syötettä.

## Katso myös

- [Komentoriviparametrien lukeminen Clojurella](https://clojure.org/guides/cli)
- [Clojure "command-line" kirjasto](https://github.com/clojure/tools.cli)
- [Java-komentoriviparametrien käsittely](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)