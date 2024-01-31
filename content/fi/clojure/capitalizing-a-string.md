---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Isoskirjaimiksi muuntaminen tarkoittaa tekstijonon muuttamista niin, että jokainen sana alkaa suurella alkukirjaimella. Ohjelmoijat käyttävät tätä tekstin ulkoasun parantamiseen ja johdonmukaisuuden ylläpitämiseen.

## How to:

Clojuren `clojure.string` -kirjasto sisältää funktioita merkkijonojen käsittelyyn. `capitalize` ja `join` yhdistelmällä saamme jokaisen sanan alkamaan isolla alkukirjaimella.

```Clojure
(require '[clojure.string :as str])

(defn capitalize-words [s]
  (str/join " " (map str/capitalize (str/split s #" "))))

(println (capitalize-words "clojure taitaa olla paras!"))
```

Tulostus:
```
"Clojure Taitaa Olla Paras!"
```

Tämä funktionaalisesti ketjuttaen:

```Clojure
(->> "clojure taitaa olla paras!"
     (str/split #" ")
     (map str/capitalize)
     (str/join " ")
     (println))

```

Tulostus edelleen:
```
"Clojure Taitaa Olla Paras!"
```

## Deep Dive

Ennen nykyaikaisten ohjelmointikielten kuten Clojuren esiintuloa, merkkijonojen käsittely oli usein melko karkeaa. Vanhemmissa kielissä, kuten C:ssä, jouduttiin käsittelemään merkkejä ja merkkijonoja manuaalisesti, joka saattoi olla työlästä ja virhealtista.

Clojuren tapaan toimivia vaihtoehtoja on monia, esimerkiksi JavaScriptin `toLowerCase()` ja `toUpperCase()` kombinaation avulla. Clojuren etu on, että sen funktiot ovat puhtaita ja se käyttää funktionaalista lähestymistapaa, mikä tekee koodista lyhyempää ja helpommin ymmärrettävää.

Clojuren `capitalize` on melko suoraviivainen. Se ottaa merkkijonon ja muuttaa ainoastaan ensimmäisen merkin isoksi, loput jäävät alkuperäisen kaltaisiksi. Tämä saattaa aiheuttaa ongelmia, jos haluamme varmistaa, että loput sanasta ovat pienellä (esim. nimenmuotoilussa). Silloin saattaa tarvita yhdistelmää `capitalize` ja `lower-case` funktioita.

```Clojure
(defn proper-name [s]
  (str/capitalize (str/lower-case s)))

(println (proper-name "sUOMI"))
```

Tulostus:
```
"Suomi"
```

## See Also

Clojure `capitalize`: https://clojuredocs.org/clojure.string/capitalize

Clojure `lower-case`: https://clojuredocs.org/clojure.string/lower-case

Clojure `map`: https://clojuredocs.org/clojure.core/map

String manipulation in Clojure: https://www.braveclojure.com/strings/
