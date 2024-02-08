---
title:                "Virheiden käsittely"
aliases:
- fi/clojure/handling-errors.md
date:                  2024-01-26T00:51:03.833169-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/handling-errors.md"
---

{{< edit_this_page >}}

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
