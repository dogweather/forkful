---
title:                "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
html_title:           "Ruby: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeen laskea tietty päivämäärä menneisyydessä tai tulevaisuudessa. Tämä voi olla esimerkiksi laskutusohjelmassa tai tapahtumakalenterissa.

## Kuinka

Määritä Rubyyn päivä, kuukausi ja vuosi `Date.new()` -menetelmällä ja tallenna se muuttujaan. Voit myös käyttää `Time.now` -metodia, jos haluat nykyisen päivämäärän. Sitten lisää haluamasi aika päälle käyttämällä `days` tai `months` -parametreja `+` -operaattorilla. Lopuksi, tulosta uusi päivämäärä `puts` -metodilla.

```Ruby
# luo uusi päivä ja tallenna se muuttujaan
paiva = Date.new(2020, 10, 1)

# lisää 30 päivää päälle
uusi_paiva = paiva + 30

# tulosta uusi päivä
puts uusi_paiva
```

Tuloste: 2020-10-31

Voit myös käyttää samankaltaista lähestymistapaa kahden päivämäärän välillä laskemiseen. Voit käyttää `Date` -luokan `>>` -operaattoria, joka antaa tuloksen päivien määrässä.

```Ruby
# luo kaksi päivää
paiva1 = Date.new(2020, 10, 1)
paiva2 = Date.new(2020, 11, 1)

# laske päivien määrä paiva1:sta paiva2:een
p = (paiva1 >> paiva2).to_i

# tulosta tulos
puts p
```

Tuloste: 31

## Deep Dive

Ruby tarjoaa useita erilaisia tapoja laskea päivämääriä tulevaisuudessa tai menneisyydessä. Voit käyttää myös `Date` -luokan muita metodeja, kuten `yesterday` tai `next_year`, saadaksesi halutun päivämäärän. Lisäksi, Rubyssä on myös mahdollista laskea eri aikavyöhykkeillä käyttämällä `DateTime` -luokkaa.

## Katso myös

- Date-luokan dokumentaatio: https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html
- Time-luokan dokumentaatio: https://ruby-doc.org/core-2.7.1/Time.html