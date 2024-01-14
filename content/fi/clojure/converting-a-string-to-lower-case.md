---
title:    "Clojure: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat muuttaa merkkijonon pienaakkosiksi Clojurella. Yksi syy voisi olla vertailun tai hakemisen helpottaminen, kun haluat varmistaa, että isot ja pienet kirjaimet eivät vaikuta tuloksiin.

## Miten

Muuttaminen isoista pieniksi kirjaimiksi Clojurella on erittäin helppoa. Voit käyttää funktiota "lower-case" ja antaa sille merkkijonon parametrina. Alla on esimerkki koodista ja tulostuksesta:

```Clojure
(def sana "Tämä ON Merkkijono")
(lower-case sana)
```

Tämä koodi tuottaa seuraavan tulosteen:

```
"tämä on merkkijono"
```

## Syvempää tietoa

Kun muutat merkkijonon pienaakkosiksi, Clojure käyttää Unicode-standardia sääntöjen mukaisesti. Tämä tarkoittaa, että myös erikoismerkit ja aktsentit muutetaan pieniksi kirjaimiksi.

Jos haluat muuntaa vain tietyt kirjaimet pieniksi, voit käyttää funktiota "clojure.string/lower-case" ja antaa sille halutun merkkijonon ja erikseen vaihtoehtoisen kielen asetuksena. Voit lukea lisää tästä toiminnosta Clojuren virallisesta dokumentaatiosta.

## Katso myös

- [Clojuren virallinen dokumentaatio](https://clojure.org/api/cheatsheet)
- [Clojuren merkkijonofunktiot](https://clojure.org/reference/strings)