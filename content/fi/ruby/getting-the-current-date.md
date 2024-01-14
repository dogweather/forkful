---
title:    "Ruby: Nykyisen päivämäärän saaminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Usein tarvitsemme nykyisen päivämäärän tietoa ohjelmointiprojekteissamme. Tämä voi olla tarpeen esimerkiksi päiväyksen lisäämiseksi tiedostojen nimiin tai tapahtumien ajoittamiseksi sovelluksissamme.

## Kuinka

Ruby-kielen `Date`-kirjasto tarjoaa meille helpon tavan saada nykyinen päivämäärä. Käytämme `::today`-metodia, joka palauttaa meille objektin, jossa on nykyinen päivämäärä.

```Ruby
# Otetaan käyttöön "Date" kirjasto
require 'date'

# Haetaan nykyinen päivämäärä
today = Date.today

# Tulostetaan päivämäärä muodossa "päivä-kuukausi-vuosi"
puts today.strftime("%d-%m-%Y")

# Tulostaa vaikka: 05-10-2020
```

Voimme myös muuttaa `strftime`-metodin avulla päivämäärän tiedoista näytettävän muodon. Esimerkiksi `"%A"` tulostaa viikonpäivän täydessä muodossa, kun taas `"%d-%m-%y"` näyttää päivän, kuukauden ja vuoden numeroina. Voit tutustua tarkemmin `strftime`-metodin tarjoamiin vaihtoehtoihin [täältä](https://www.rubydoc.info/stdlib/date/Date#strftime-instance_method).

## Syvällisempi sukellus

Päivämäärän saaminen ei olekaan niin yksinkertaista kuin voisi kuvitella. Useiden tekijöiden, kuten aikavyöhykkeiden ja päivämäärämuotoilujen, vuoksi päivämäärän hankkiminen voi aiheuttaa haasteita.

Ruby tarjoaa onneksi monipuolisia työkaluja päivämäärän hallintaan. `Date`-kirjaston lisäksi meillä on myös `Time`-kirjasto sekä lukuisia apumetodeja, kuten `parse`, joilla voimme helposti käsitellä päivämäärätietoja.

## Katso myös

- [Ruby Date-kirjaston virallinen dokumentaatio](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Ruby Time-kirjaston virallinen dokumentaatio](https://ruby-doc.org/core-2.7.1/Time.html)
- [strftime-metodin vaihtoehdot](https://www.rubydoc.info/stdlib/date/Date#strftime-instance_method)