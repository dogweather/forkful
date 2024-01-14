---
title:    "Clojure: Allaolevien alimerkkien erottaminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi: Miksi kannattaa ryhtyä erottamaan alirivejä?

Erottamalla alirivejä, voimme käsitellä merkkijonoja helposti ja tehokkaasti. Tämä auttaa meitä muokkaamaan tietoja ja suorittamaan monimutkaisempia tehtäviä, kuten tekstianalyysiä tai tekstipohjaista hakua.

## Ohjeet: Näin erottat alirivejä Clojurella

```Clojure
; Määritetään merkkijono
(def s "Tervetuloa Clojureen")

; Käytetään `subs` -funktiota erottamaan alirivi
; Arvot 6 ja 12 määrittävät aloitus- ja loppukohteen
(subs s 6 12)

; Tulostaa "Clojure"
```

## Syvällinen sukellus: Tietoa alirivien erottamisesta

Alirivien erottaminen on helposti tehtävissä Clojurella käyttämällä `subs` -funktiota. Tämä funktio ottaa kolme parametria: merkkijonon, aloituskohdan ja loppukohdan. Aloitus- ja loppukohdat voivat olla joko numeroita tai merkkijonoja. Huomaa, että aloitusindeksi on mukaanlukien, mutta loppuindeksi ei ole.

```subs``` -funktiolla voimme myös erottaa alirivejä loppuun asti, jättämällä loppukohtaan parametriksi "-1". Voimme myös käyttää negatiivisia indeksejä, jotka lasketaan taaksepäin merkkijonon lopusta. Esimerkiksi käyttämällä aloitusindeksiä -6 ja loppuindeksiä -1, erottaisimme "Clojureen" merkkijonon lopusta alkuosan "Clojure".

## Katso myös
- [Clojure Docs: String Functions](https://clojure.org/guides/learn/compilation#strings)
- [ClojureDocs: subs](https://clojuredocs.org/clojure.core/subs)