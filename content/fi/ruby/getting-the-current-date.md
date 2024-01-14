---
title:                "Ruby: Nykyisen päivämäärän hankkiminen"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi?

Ruby on mahtava ohjelmointikieli, joka tarjoaa monia käteviä ominaisuuksia. Yksi näistä ominaisuuksista on kyky hakea nykyinen päivämäärä. Tässä blogitekstissä tarkastelemme, miksi ja miten voit käyttää tätä ominaisuutta omassa Ruby-ohjelmassasi.

## Miten?

Voit helposti hakea nykyisen päivämäärän Rubyssa käyttämällä `Date` luokkaa. Tämä luokka sisältää useita hyödyllisiä metodeja, joita voit käyttää päivämäärien hallitsemiseen.

```Ruby
require 'date'

today = Date.today
puts today
```

Tämä koodi tulostaisi nykyisen päivämäärän muodossa `yyyy-mm-dd`.

Voit myös halutessasi muuttaa päivämäärän haluamaasi muotoon käyttämällä `strftime`-metodia:

```Ruby
puts today.strftime("%d.%m.%Y")
```

Tämä koodi tulostaisi päivämäärän muodossa `dd.mm.yyyy`. `strftime` on erittäin hyödyllinen metodi, koska sen avulla voit muotoilla päivämäärän haluamallasi tavalla.

## Syvemmälle

Ruby tarjoaa myös mahdollisuuden käyttää tarkempia ajan ja päivämäärän ominaisuuksia `Time` luokan avulla. Tämä luokka sisältää metodeja, kuten `now` ja `localtime`, jotka antavat tarkemman ajan ja päivämäärän tiedot.

Voit myös käyttää `Time` luokkaa saadaksesi päivämäärän tietyn aikavyöhykkeen perusteella käyttämällä `utc`-metodia:

```Ruby
time_in_ny = Time.now.utc
puts time_in_ny
```

Tämä koodi tulostaisi nykyisen päivämäärän ja ajan käyttäen UTC-aikavyöhykettä.

## Katso myös

- [Ruby'n virallinen dokumentaatio päivämäärän ja ajan hallitsemisesta](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Ruby'n opetusohjelmat päivämäärän ja ajan käytöstä](https://www.rubyguides.com/2019/02/ruby-date-time-and-datetime/)
- [Lyhyt opas Ruby'n `strftime`-metodin käyttöön](https://gist.github.com/jackiekazil/6201722)