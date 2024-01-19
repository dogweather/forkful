---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän jäsennys merkkijonosta tarkoittaa päivämäärän hahmottamista merkkijonosta. Ohjelmoijat tekevät tätä käyttäjän syötteiden, tiedostojen tai tietokannan päivämäärämuotoilujen ymmärtämiseksi. 

## Kuinka Näin:

Jäsentää päivämäärä merkkijonosta Clojuren avulla:

```Clojure 
(require '[clj-time.format :as f])
(def iso-formatter (f/formatters :date-time-no-ms))
(f/parse iso-formatter "2012-12-12T12:12:12Z")
```

Tulostaa tuloksen: `#object[org.joda.time.DateTime 2012-12-12T14:12:12.000Z]`, päivämäärä- ja aikaolio vakiomuodossa.

Mutta entä jos meillä on muoto, jota Clojure ei ymmärrä oletusarvoisesti? Ei hätää, voit määritellä oman muodon:
```Clojure 
(def custom-formatter (f/formatter "dd-MM-yyyy"))
(f/parse custom-formatter "12-12-2012")
```
Tämä tuottaa saman tuloksen kuin edellinen, mutta nyt muotoillaan päivämäärä omalla muodolla.

## Syvempi sukellus

Alun perin, ohjelmoijat käyttivät JavaScriptin pudotetun Date.parse-toiminnon, jolla on paljon haittoja, kuten epäjohdonmukaisuus selaimen alustoilla. Clojure tarjoaa selkeämmän ja joustavamman lähestymistavan, joka perustuu Joda-Time-kirjastoon, täynnä erilaisia aika- ja päivämääränmuuntoja ja -muotoja.

Vaihtoehtoisia tapoja päivämäärän jäsennystä varten merkkijonosta ovat muun muassa Java.util.Date ja SimpleDateFormat, mutta ne ovat suhteellisen monimutkaisia käyttää ja niillä on monia aikavyöhykkeisiin liittyviä kysymyksiä.

Clojuren toteutus käyttää sisäisesti Joda-Time-menetelmiä, jotka tekevät siitä vakaan ja tehokkaan. Se määrittää formattereita käyttäen DateTimeFormat-luokan menetelmää, joka palauttaa DateTimeFormatter-olioita - nämä voivat valita parse-strategian merkkijonojen perusteella.

## Katso myös

Linkit liittyviin lähteisiin:
- [Clojure Date Time Documentation](https://clojuredocs.org/clojure.instant)
- [Joda-Time Github](https://github.com/JodaOrg/joda-time)
- [More on Time in Clojure](https://www.baeldung.com/clojure-time-and-date)