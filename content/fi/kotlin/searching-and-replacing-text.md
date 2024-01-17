---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Kotlin: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Etsiminen ja tekstin korvaaminen ovat tärkeitä osia ohjelmoinnissa. Näitä työkaluja käytetään löytämään tiettyjä merkkijonoihin liittyviä tietoja ja korvaamaan ne halutuilla muutoksilla. Tämä voi auttaa säästämään aikaa ja vaivaa, kun työskentelet suurten tietomäärien kanssa.

Ohjelmoijien on usein tarpeen muuttaa tietokannan tietoja tai käsitellä suuria määriä tekstidataa. Etsimisen ja korvaamisen avulla nämä prosessit voidaan automatisoida ja ne voidaan suorittaa helposti ilman manuaalista työtä.

# Miten tehdä?

Kotlin tarjoaa helpon tavan hakea ja korvata tekstiä käyttäen String-luokan funktioita. Voit käyttää funktiota ```replace()``` korvataksesi tietyn merkkijonon toisella merkkijonolla ja ```contains()``` tarkistamaan, sisältääkö merkkijono halutun tekstin.

Esimerkiksi seuraava koodi etsii ja korvaa kaikki puhelinnumerot merkkijonossa:

```Kotlin
val teksti = "Puhelinnumeroni on +358 123 456 789."
val uusiTeksti = teksti.replace("+358", "0") // uusiTeksti on nyt "Puhelinnumeroni on 0 123 456 789."
```

Voit myös käyttää näitä funktioita yhdessä ```if```-lauseen kanssa tarkistaaksesi, sisältääkö merkkijono halutun tekstin:

```Kotlin
val teksti = "Tämä on salainen viesti."
if (teksti.contains("salainen")) {
  println("Sisältää salaisen viestin!")
}
```

# Syväsukellus

Etsimisen ja korvaamisen käsitteet ovat olleet läsnä ohjelmoinnissa alusta asti. Alkuaikoina käytettiin usein Unix-komentoja, kuten ```grep``` ja ```sed```, jotka toimivat samalla periaatteella kuin nykyaikaiset ohjelmointikielten funktiot.

Kotlin lisäksi tarjoaa myös muita käteviä funktioita, kuten ```substring()``` ja ```matches()```. On myös olemassa vaihtoehtoisia tapoja ratkaista ongelmia, kuten käyttämällä säännöllisiä lausekkeita tai erilaisia tekstinkäsittelykirjastoja.

Tekstin etsimisen ja korvaamisen toteutukseen vaikuttaa myös suorituskyky ja muistinkäyttö. Joissakin tapauksissa erilaiset toteutukset voivat vaikuttaa merkittävästi koodin tehokkuuteen ja suoritusaikaan.

# Katso myös

- [Kotlinin String-luokan dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Ohjeita tekstinkäsittelyyn Kotlinilla](https://kotlinexpertise.com/kotlin-text-manipulation/)