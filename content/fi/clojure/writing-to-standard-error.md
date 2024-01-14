---
title:    "Clojure: Kirjoittaminen standardivirheelle"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Miksi kirjoittaa standardiin virheeseen?

Kirjoittaminen standardiin virheeseen eli standard error (stderr) on tärkeä osa Clojure-ohjelmointia. Se auttaa ohjelmoijia havaitsemaan virheitä ja suorituksen ongelmia ohjelman suorittamisen aikana. Kirjoittamalla tiedot standardiin virheeseen, saadaan tarvittava tieto siitä, mitä ohjelma tekee suorituksen aikana ja mahdollisista ongelmista, jotka aiheuttavat sen kaatumisen.

# Kuinka kirjoittaa standardiin virheeseen?

Standardiin virheeseen kirjoittaminen tapahtuu käyttämällä Clojuren `println`-funktiota ja antamalla sille tieto, joka halutaan kirjoittaa. Tämän jälkeen tieto näkyy standardi virheessä ohjelman suorituksen aikana.

```Clojure
(println "Tämä on virheilmoitus standardiin virheeseen!")
```

Lopputulos näyttää tältä:

```Clojure
Tämä on virheilmoitus standardiin virheeseen!
```

Jos halutaan lisätä enemmän tietoa, voidaan käyttää muuttujia ja yhdistää niitä tekstiin. Esimerkiksi:

```Clojure
(def nimi "Hanna")
(println "Tämä on" nimi "'n virheilmoitus!")
```

Tämän tuloksena saadaan:

```Clojure
Tämä on Hanna'n virheilmoitus!
```

# Syvempi sukellus

Kun ohjelma suoritetaan, tietoja lähetetään kahteen eri paikkaan: standardiin tulosteeseen ja standardiin virheeseen. Standardi tulosteeseen lähetetään normaali ohjelman suorituksen aikana tulostettava tieto, kun taas standardi virheeseen lähetetään virheilmoitukset ja muut hälytykset.

Tärkeää on huomata, että standardi virheeseen kirjoittaminen ei estä ohjelmaa suorittamasta loppuun asti. Tämän ansiosta virheilmoitukset ja muut hälytykset pystytään tallentamaan ja lukemaan myöhemmin, mikä auttaa virhetilanteiden selvittämisessä ja korjaamisessa.

# Katso myös

- [Clojure - Virheiden käsittely](https://clojure.org/reference/exceptions)
- [Standardi virheen hallinta Clojurella](https://www.rfc1149.net/blog/2011/05/07/clojure-standard-error-management/)
- [Clojure Standardi kirjasto](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/println)