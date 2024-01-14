---
title:    "Clojure: Pilkkujen poistaminen vastaavaa kaavaa vastaavista merkeistä"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Tämä voi olla hyödyllistä esimerkiksi jos haluat siivota tekstidokumenttiasi tai käsitellä käyttäjän antamia syötteitä ennen niiden tallentamista tietokantaan.

## Kuinka

```Clojure
; Ensimmäinen vaihtoehto: käyttämällä replace funktiota
(replace #"kaava" "korvattava teksti" "korvaava teksti")

; Toinen vaihtoehto: käyttämällä regex-replace funktiota
(regex-replace #"kaava" "korvattava teksti" "korvaava teksti")
```

**Esimerkki**: Poistetaan kaikki välilyönnit lauseen lopusta:
```Clojure
(replace #"\s+$" "Tämä on esimerkki  ") ; Output: "Tämä on esimerkki"
(regex-replace #"\s+$" "Tämä on esimerkki  ") ; Output: "Tämä on esimerkki"
```

## Syväsukellus

Kun käytät replace- tai regex-replace-funktioita, voit käyttää erilaisia kaavoja poistettavien merkkien tunnistamiseksi. Esimerkiksi "#\s" vastaa välilyönnin merkkiä ja "$" vastaa rivin loppua. Voit myös käyttää vakioita, kuten :ignore, :globs tai :exact, jotka auttavat tunnistamaan kaavaa tarkemmin.

## Katso myös

- [Clojure replace documentation](https://clojuredocs.org/clojure.string/replace)
- [Clojure regex-replace documentation](https://clojuredocs.org/clojure.string/regex-replace)