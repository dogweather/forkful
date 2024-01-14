---
title:    "Clojure: Pääkomennoin argumenttien lukeminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Miksi lukea komentoriviargumentteja? Komentoriviargumenttien lukeminen on tärkeä taito, joka auttaa ohjelmoijia paremmin hallitsemaan ja suorittamaan heidän luomiaan ohjelmia. Valmiiksi annettuja argumentteja käyttämällä ohjelmoijat voivat määrittää ohjelmiensa suorituksen eri parametreilla ja tehdä siitä joustavamman.

## Miten
Komentoriviargumenttien lukeminen on helppo toteuttaa Clojuren avulla. Alla on esimerkkejä koodista ja tulosteesta:

```Clojure
;; Määritetään muuttuja komentoriviargumenteille
(def args *command-line-args*)
 
;; Tulostetaan argumentit
(print args)
```

Kun ajetaan komennolla `clj -m args "Hello" "world!"`, koodin tulosteena nähdään `"Hello" "world!"`. Käyttämällä tätä tapaa, ohjelmoija voi lukea ja käyttää komentoriviargumentteja koodissaan.

## Syvällisempi tarkastelu
Komentoriviargumenttien lukeminen onnistuu myös käyttämällä `clojure.core` -kirjaston `with-command-line` funktiota. Tämä mahdollistaa argumenttien käsittelyn ennen niiden käyttöä koodissa. Alla on esimerkki koodista:

```Clojure
;; Määritetään argumentit käytettäviksi muuttujissa
(with-command-line args
  (let [hello (first args)
        name (second args)]
    (println "Päivää," name "oletko kuullut tervehdyksestä" hello "?")))
```

Kun ajetaan komennolla `clj -m args "Moi" "Sakura"`, koodin tulosteena nähdään `"Päivää, Sakura oletko kuullut tervehdyksestä Moi?"`.

## Katso myös
- [Clojure Docs: with-command-line](https://clojuredocs.org/clojure.core/with-command-line)
- [Brave Clojure: Command-Line Arguments](https://www.braveclojure.com/functional-programming/?genie=querystring&arg0=command-line%20arguments&arg1=eb98842c-69aa-42b4-8331-3cbc20404c51&arg2=5c8894cd-9b50-44ea-8928-f177017e846b&arg3=https%3A%2F%2Fwww.braveclojure.com%2Ffunctional-programming%2F&arg4=1521704251824&arg5=0.9999999999968891)
- [The Clojure Style Guide: Command-Line Arguments](https://guide.clojure.style/dynamic-programming/command-line.html)