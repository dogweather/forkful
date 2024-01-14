---
title:                "Clojure: Tarkistetaan onko hakemisto olemassa"
simple_title:         "Tarkistetaan onko hakemisto olemassa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Usein Clojure-ohjelmoijille voi tulla tarve tarkistaa, onko jokin kansio olemassa ohjelman suorituksen aikana. Tämä voi olla tarpeellista esimerkiksi ennen tiedostojen hakemista tai tallentamista tiettyyn sijaintiin. Tässä blogipostauksessa käydään läpi kuinka tarkistaa hakemiston olemassaolo Clojurella.

## Miten

Tarkistaaksesi, onko hakemisto olemassa, voit käyttää Clojuren `clojure.java.io/file` -funktiota yhdessä `java.io.File` -luokan kanssa. Tämä funktio palauttaa tiedoston olion, joka kuvaa hakemistoa, ja voit tarkistaa sen olemassaolon käyttämällä `java.io.File/exists?` -metodia.

```Clojure
(def directory (clojure.java.io/file "/polku/hakemistoon/"))
(if (.exists directory)
  (println "Hakemisto löytyi!")
  (println "Hakemistoa ei löytynyt.")
  )
```

Tämä esimerkki olettaa, että `/polku/hakemistoon/` on olemassa. Jos hakemistoa ei löydy, tulostetaan `Hakemistoa ei löytynyt.`

## Syventävä tarkastelu

Tarkistaessaan hakemiston olemassaoloa `java.io.File/exists?` käyttää `java.io.File/exists` -metodia, joka puolestaan kutsuu samannimistä Javan metodia. Tämä metodi tarkistaa, onko tiedosto olemassa ja palauttaa `true` jos tiedosto löytyy tai `false` jos tiedostoa ei löydy. Sen sijaan `java.io.File` -luokan `exists?` -metodi palauttaa `nil` jos tiedostoa ei löydy ja tiedoston olion, jos tiedosto löytyy.

On myös tärkeää huomata, että `java.io.File/exists?` ei tarkista pelkästään hakemiston olemassaoloa, vaan myös mahdollisia käyttöoikeuksia. Jos käyttäjällä ei ole oikeutta tarkistaa hakemistoa, `exists?` palauttaa `false` vaikka hakemisto olisikin olemassa.

## Katso myös

- [Clojure - `clojure.java.io/file`](https://clojuredocs.org/clojure.java.io/file)
- [Java - `java.io.File`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)