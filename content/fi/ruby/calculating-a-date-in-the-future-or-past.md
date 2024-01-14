---
title:    "Ruby: Tulevan tai menneen päivämäärän laskeminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi laskea päivämäärän tulevaisuudessa tai menneisyydessä? Monissa ohjelmointiprojekteissa on tarpeen laskea tietty päivämäärä jostain toisesta päivämäärästä eteen- tai taaksepäin. Tämä voi olla hyödyllistä esimerkiksi laskutuksessa, aikataulutuksessa tai laskennallisessa analyysissä.

## Miten

Ruby tarjoaa kätevän tavan laskea päivämääräksi tietyn määrän päiviä tai kuukausia eteen- tai taaksepäin. Tätä varten käytämme `Time` -luokkaa ja sen metodeita `#advance` ja `#change`. Tarkastellaanpa seuraavaa esimerkkiä:

```Ruby
# Lasketaan päivämäärä 30 päivää eteenpäin tänään
tanaan = Time.now
uusi_paivamaara = tanaan.advance(days: 30)

puts uusi_paivamaara
# output: 2021-08-31 11:00:00 +0300

# Lasketaan päivämäärä 6 kuukautta taaksepäin tänään
uusi_paivamaara = tanaan.advance(months: -6)

puts uusi_paivamaara
# output: 2020-02-01 11:00:00 +0200

# Lasketaan päivämäärä joulukuuhun tänä vuonna
uusi_paivamaara = tanaan.change(month: 12)

puts uusi_paivamaara
# output: 2020-12-01 11:00:00 +0200
```

## Syvempi sukellus

`#advance` -metodi hyväksyy lukuisia parametreja, kuten `years`, `weeks` ja `seconds`. Voit myös välittää siihen useita arvoja yhdellä kertaa. Esimerkiksi `tanaan.advance(months: 3, weeks: 2)` laskee päivämäärän 3 kuukautta ja 2 viikkoa eteenpäin tänään.

`#change` -metodi puolestaan antaa sinun muuttaa tiettyä osaa päivämäärästä, kuten kuukautta, päivää tai vuotta. Voit myös yhdistää `#change` ja `#advance` -metodit, jolloin voit laskea päivämäärän esimerkiksi seuraavan tiistain päälle tai edellisen kuukauden ensimmäiselle päivälle.

## Katso myös

- [Ruby dokumentaatio: Time luokka](https://ruby-doc.org/core-2.7.3/Time.html)
- [Ruby on Rails dokumentaatio: ActiveSupport::TimeWithZone](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
- [Date ja DateTime luokat](https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html)