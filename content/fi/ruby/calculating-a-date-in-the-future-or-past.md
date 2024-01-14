---
title:                "Ruby: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointityössä tarvitaan kykyä laskea päivämääriä tulevaisuuteen tai menneisyyteen. Tämä voi olla hyödyllistä esimerkiksi tapahtumien aikataulutuksessa tai laskuissa. Onneksi Rubyn avulla tämä on tehty helpoksi!

## Miten

Ehdotan seuraavaa lähestymistapaa päivämäärien laskemiseen tulevaisuuteen tai menneisyyteen:

```Ruby
# Laske päivämäärä kahden viikon päähän
päivä = Date.today + 2.weeks
```
Tulostettaisi tämä koodin osa näyttäisi tältä: "2020-09-16". 

Tässä esimerkissä käytämme Date.luokkaa ja sen ominaisuuksia lisätäksemme kaksi viikkoa tämänhetkiseen päivämäärään. Voit myös lisätä tai vähentää yksittäisiä päiviä, kuukausia tai vuosia halutun päivämäärän perusteella.

```Ruby
# Laske päivämäärä seitsemän päivää menneisyyteen
päivä = Date.today - 1.weeks
```
Tulostettaisi tämä koodin osa näyttäisi tältä: "2020-09-02".

Voit myös luoda tarkempia päivämääriä käyttämällä Time.luokkaa. Seuraavassa esimerkissä lisäämme 5 päivää vuoden 2021 toukokuun ensimmäiselle päivälle:

```ruby
# Luodaan päivämäärä ensi vuodeksi, toukokuun ensimmäinen päivä
päivä = Time.local(2021, 5, 1) + 5.days
```
Tulostettaisi tämä koodin osa näyttäisi tältä: "2021-05-06 00:00:00 +0300".

## Syvä Sukellus

Nyt kun tiedämme, kuinka päivämääriä lasketaan Rubylla, voimme tarkastella hieman lisätietoja. Rubyn Date.luokalla on monia sisäänrakennettuja metodeja, joiden avulla voit muuttaa ja manipuloida päivämääriä. Näitä ovat muun muassa .next_day, .prev_day, .next_month ja .prev_month.

Voit myös tarkastella tietyn päivän viikonpäivää käyttämällä .wday -metodia. Esimerkiksi jos haluat tietää, mikä päivä viikon ensi kuussa on sunnuntai, voit tehdä seuraavaa:

```ruby
# Tarkista ensi kuukauden sunnuntait
date = Date.today.next_month
p date.sunday?
```
Tulostettaisi tämä koodin osa näyttäisi tältä: "2020-10-11 00:00:00 +0300". Olemme siis tulleet tulokseen, että ensi kuukauden ensimmäinen sunnuntai on 11. päivä.

## Katso myös

- [Rubyn dokumentaatio Date.luokasta](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Vuorovaikutteinen Rubyn oppimispeli päivämäärien laskemisesta](https://rubyinside.com/days-elapsed-1097.html)