---
title:                "Ruby: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi sinun kannattaisi oppia hakemaan nykyinen päivämäärä Ruby-ohjelmoinnilla. Ehkä haluat lisätä päivämäärän johonkin peliin tai sovellukseen, tai ehkä haluat vain näyttää sen käyttäjille.

## Miten

Hanki nykyinen päivämäärä Rubyssa voi käyttämällä Date-luokkaa ja sen metodeja. Ensiksi, sinun täytyy vaatia Date-muotoiluaohjelman ulkopuolelle:

```Ruby
require 'date'
```

Sitten, voit käyttää `Date.today` -metodia saadaksesi nykyisen päivämäärän:

```Ruby
Date.today
```

Tämän komennon suorittamisen jälkeen, saat takaisin nykyisen päivämäärän muodossa "YYYY-MM-DD". Esimerkiksi, jos tänään on 15. syyskuuta 2021, saat seuraavan tulosteen:

```
2021-09-15
```

Halutessasi voit myös muuttaa päivämäärän muotoa käyttämällä `strftime` -metodia. Esimerkiksi, jos haluat tulostaa päivämäärän muodossa "DD.MM.YYYY", voit tehdä sen seuraavalla tavalla:

```Ruby
Date.today.strftime("%d.%m.%Y")
```

Tämä antaa sinulle tuloksen "15.09.2021". Voit käyttää muita muotoilumerkkejä saadaksesi haluamasi päivämäärän muodon. Voit lukea lisää Date-luokasta ja sen metodeista [Ruby-opas](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html).

## Syvempi sukellus

Rubyssa on myös muita tapoja saada nykyinen päivämäärä, kuten käyttämällä `Time` ja `DateTime` -luokkia. Näissä luokissa on myös omat metodejaan nykyisen päivämäärän hakemiseksi ja muotoilemiseksi. Voit lukea lisää näistä vaihtoehdoista [Ruby-oppaasta](https://ruby-doc.org/core-2.7.2/Time.html) ja [Ruby-oppaasta](https://ruby-doc.org/stdlib-2.7.2/libdoc/datetime/rdoc/DateTime.html).

## Katso myös

 - [Ruby-opas](https://www.ruby-lang.org/en/)
 - [Ruby-ohjelmointikurssit](https://www.coursereport.com/tracks/ruby-on-rails-courses?_ga=2.2670917.506796776.1631727791-1361597607.1631727791)
 - [Ruby-keskustelufoorumi](https://www.ruby-forum.com/)
 - [Ruby-yhteisön blogit](https://www.ruby-lang.org/en/news/)