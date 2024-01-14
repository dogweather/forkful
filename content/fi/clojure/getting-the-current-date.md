---
title:                "Clojure: Nykyisen päivämäärän hakeminen"
simple_title:         "Nykyisen päivämäärän hakeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Miksi päivämäärän hankkiminen on tärkeää

Päivämäärän hankkiminen (tai nykyisen päivän hankkiminen) on tärkeää monissa ohjelmoinnin yhteyksissä. Se voi olla hyödyllistä esimerkiksi aikaleimojen luomisessa, aikataulujen tarkistamisessa tai raporttien luomisessa. Clojure tarjoaa helpon ja tehokkaan tavan hankkia nykyinen päivämäärä ja tässä blogissa kerromme, miten se tapahtuu.

# Kuinka hankkia nykyinen päivämäärä Clojurella

Clojure tarjoaa built-in toiminnon `clojure.java-time/local-date` nykyisen päivämäärän hankkimiseen. Voit käyttää sitä suoraan ilman lisäkirjastoja tai asennuksia. Alla on esimerkki koodista, joka hakee ja tulostaa nykyisen päivämäärän:

```Clojure
(clojure.java-time/local-date)
; => #object[java.time.LocalDate 0x1b76efb1 "2019-06-10"]
```

Kuten näet, funktio palauttaa `java.time.LocalDate` -objektin, joka sisältää tiedot päivämäärästä. Voit käyttää Clojuren tarjoamia toimintoja tämän objektin tietojen käsittelyyn, esimerkiksi `(.getYear date)` hakee vuodet ja `(.getMonth date)` kuukaudet.

Voit myös käyttää `clojure.java-time` -kirjaston muita funktioita hieman monimutkaisempien päivämäärien hankkimiseen, kuten esimerkiksi aikavyöhykesidonnaisten päivämäärien käsittelyyn. Tarkista Clojuren virallinen dokumentaatio lisätietoja varten.

# Syvempi sukellus päivämäärän hankkimiseen

Nykyisen päivämäärän hankkiminen on helppoa, mutta takana on paljon enemmän kuin vain yksi funktio. Clojure pohjautuu vahvasti Javan `java.time` -ajankohtakeskukselle, joten päivämäärän käsittely Clojurella on todella tehokasta ja monipuolista. Voit esimerkiksi luoda uuden päivämäärän, lisätä tai vähentää päiviä, tarkistaa päivämäärien välistä eroa ja paljon muuta.

# Katso myös

- Clojuren virallinen dokumentaatio päivämäärän käsittelyyn: https://clojure.github.io/java-time/
- Java:n `java.time` -ajankohtakeskus: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html