---
title:                "Ruby: Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kannattaisi muuttaa päivämäärä merkkijonoksi? Vaikka tätä toimintoa ei välttämättä tarvitse jokapäiväisessä ohjelmoinnissa, se voi olla hyödyllinen taito, jolla voit esimerkiksi näyttää päivämäärän käyttäjälle ymmärrettävässä muodossa tai tallentaa päivämäärää tietokantaan.

## Miten

Päivämäärän muuttaminen merkkijonoksi Rubylla on melko helppoa. Tarvitsemme vain päivämäärämuuttujan ja käytämme siihen `to_s` -metodia. Tässä on yksinkertainen esimerkki:

```Ruby
päivä = Time.now
puts päivä.to_s
```
Output: "2021-07-29 14:53:18 +0300"

Voimme myös muuttaa päivämäärän haluamaamme muotoon käyttämällä `strftime` -metodia, joka antaa meille enemmän hallintaa merkkijonon muotoilussa. Tässä esimerkki, jossa päivä näytetään muodossa "25.7.2021":

```Ruby
päivä = Time.new(2021, 7, 25)
puts päivä.strftime("%d.%m.%Y")
```
Output: "25.07.2021"

## Syväsyvennys

Päivämäärän muuttaminen merkkijonoksi perustuu Rubyssa `to_s` -metodiin, joka on määritelty `Object` -luokassa. `to_s` -metodi on osa Rubyn automaattista jäsennystä ja se kutsutaan automaattisesti, kun muunnat muuttujan merkkijonoksi.

Voimme myös muokata `to_s` -metodia haluamallamme tavalla, jos tarvitsemme tarkempaa hallintaa merkkijonon muotoilussa. Rubyssa on myös muita metodeja, kuten `strftime` ja `to_datetime`, jotka auttavat päivämäärän muuntamisessa merkkijonoksi.

## Katso myös

- [Ruby:n DateTime-luokka](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/DateTime.html)
- [anerush/ruby-date-formatting-cheatsheet](https://github.com/anerush/ruby-date-formatting-cheatsheet)
- [Ruby:n virallinen dokumentaatio](https://www.ruby-lang.org/en/documentation/)