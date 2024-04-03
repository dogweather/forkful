---
date: 2024-01-26 00:51:03.833169-07:00
description: "Virheenk\xE4sittely on ohjelmien odottamattomien tilanteiden hallintaa\u2014\
  kuin j\xE4rjestyksenvalvoja, joka kohtaa h\xE4irik\xF6it\xE4. Ohjelmoijat pit\xE4\
  v\xE4t sujuvuudesta;\u2026"
lastmod: '2024-03-13T22:44:56.191522-06:00'
model: gpt-4-1106-preview
summary: "Virheenk\xE4sittely on ohjelmien odottamattomien tilanteiden hallintaa\u2014\
  kuin j\xE4rjestyksenvalvoja, joka kohtaa h\xE4irik\xF6it\xE4."
title: "Virheiden k\xE4sittely"
weight: 16
---

## Mikä & Miksi?
Virheenkäsittely on ohjelmien odottamattomien tilanteiden hallintaa—kuin järjestyksenvalvoja, joka kohtaa häiriköitä. Ohjelmoijat pitävät sujuvuudesta; virheenkäsittely auttaa pitämään ongelmat kurissa ja varmistaa, että koodi ei kompastu ja kaadu kohdatessaan odottamatonta.

## Miten:
Clojure, kuten sen Lisp-esivanhemmat, nojaa poikkeusten varaan virheiden käsittelyssä. Tässä on miten näytät mihin olet kykeneväinen, kun asiat menevät pieleen.

Poikkeuksen heittäminen on suoraviivaista:
```Clojure
(throw (Exception. "Voi! Jotain meni pieleen."))
```

Poikkeuksen ottaminen kiinni—tätä tulet tekemään paljon:
```Clojure
(try
  ;; riskialtis koodi
  (/ 1 0)
  (catch ArithmeticException e
    (println "Ei voi jakaa nollalla!"))
  ;; finally-lohko suoritetaan joka tapauksessa
  (finally 
    (println "Siivouskoodi tulee tähän.")))
```
Esimerkkituloste yllä olevasta catch-lohkosta:
```
Ei voi jakaa nollalla!
Siivouskoodi tulee tähän.
```

Käyttäen `ex-info` ja `ex-data` rikastamaan poikkeusten kontekstia:
```Clojure
(try
  ;; aiheuttaen mukautetun poikkeuksen
  (throw (ex-info "Mukautettu virhe" {:type :custom-failure}))
  (catch Exception e
    ;; otetaan data ulos mukautetusta poikkeuksesta
    (println (ex-data e))))
```
Esimerkkituloste:
```
{:type :custom-failure}
```

## Syväsukellus
Virheenkäsittelytarina Clojuressa ei radikaalisti eroa muista Lispeistä tai edes Javasta (jolta se perii `try-catch`-mekanismin). Se on pragmaattinen; poikkeusten käyttö on päälinja, aivan kuten Javassa, mutta Clojure tarjoaa funktionaalisen maun `ex-info` ja `ex-data` avulla rikkaamman virhedatan myötä.

Vaihtoehdot virheenkäsittelyyn Clojuressa sisältävät monadiset rakenteet, kuten `either`-monadi kirjastoista kuten `cats`, tai core.async kanavapohjaiseen virheiden etenemiseen. Nämä ovat kuitenkin monimutkaisempia ja niitä käytetään erityistilanteissa.

Historiallisesti virheenkäsittely ohjelmointikielissä on kehittynyt yksinkertaisista statuspalautuksista nykykielten monimutkaisempiin poikkeustenkäsittelymekanismeihin. Clojure valitsee yksinkertaisuuden ja funktionaalisen ohjelmoinnin ripauksen, yhdistäen vanhaa ja uutta.

## Katso Myös
- Clojuren opas poikkeuksille: https://clojure.org/guides/exceptions
- “Cats”-kirjasto funktionaalisempiin lähestymistapoihin: https://github.com/funcool/cats
- “Core.async” asynkroniseen ohjelmointiin: https://github.com/clojure/core.async
