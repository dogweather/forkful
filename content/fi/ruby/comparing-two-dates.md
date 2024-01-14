---
title:                "Ruby: Kahden päivämäärän vertaaminen"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Useat ohjelmoijat joutuvat ajoittain vertailemaan kahta päivämäärää keskenään. Tämä voi johtua esimerkiksi tarpeesta laskea päivien tai viikkojen välinen ero, tai tarkistaa, kumpi päivämäärä on ennen tai jälkeen toista. Tässä blogikirjoituksessa käymme läpi, miten voit vertailla päivämääriä Ruby-ohjelmointikielellä.

## Miten

Vertaillessa kahta päivämäärää, on tärkeää muistaa että ne ovat Rubyssa Date-olioita. Tämä tarkoittaa, että niitä voidaan käsitellä samalla tavalla kuin muitakin olioita, ja niillä on käytössään erilaisia metodeja.

Käytännössä päivämäärien vertailu tapahtuu niiden välisenä matemaattisena operaationa. Tämä tarkoittaa, että voit käyttää merkkejä >, < ja == vertailuoperaattoreina päivämäärien välillä.

Esimerkiksi, jos haluat tarkistaa, onko tietty päivämäärä ennen toista, voit käyttää seuraavaa koodia:

```Ruby
date1 = Date.new(2021, 5, 10)
date2 = Date.new(2021, 5, 15)

if date1 < date2
  puts "Date1 on ennen Date2:ta"
end
```

Tulostus tästä koodista olisi:

```
Date1 on ennen Date2:ta
```

Voit myös laskea kahden päivämäärän välisen eron käyttämällä Date-olioille tarjolla olevaa `days`-metodia. Tämä antaa tulokseksi päivien määrän kahden päivämäärän välillä.

```Ruby
days = date2 - date1

puts "Päivien määrä Date1 ja Date2 välillä on #{days}"
```

Tulostus tästä koodista olisi:

```
Päivien määrä Date1 ja Date2 välillä on 5
```

## Syväsukellus

Rubyssä päivämäärät ovat tallennettuna Gregoriaaniseen kalenteriin, joten voit verrata niitä helposti muihin kalentereihin. Voit esimerkiksi muuntaa päivämäärän esimerkiksi Heprean tai Islamilaisen kalenterin mukaan käyttämällä `gregorian`-metodia ja antamalla sille haluamasi kalenterin nimen parametrina.

Rubyssa on myös tarjolla erilaisia Date-olioihin liittyviä metodeja, jotka auttavat päivämäärien vertailussa ja muokkauksessa. Näitä ovat mm. `next`, `prev` ja `to_s`.

## Katso myös

- [Ruby Date -dokumentaatio](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby Time-luokan vertailuoperaattorit](https://ruby-doc.org/core-2.7.2/DateTime.html#method-i-3C-3C)
- [Ruby Date-formatoinnin opas](https://strftime.org/)