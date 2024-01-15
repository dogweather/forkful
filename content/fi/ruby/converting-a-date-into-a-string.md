---
title:                "Päivämäärän muuttaminen merkkijonoksi."
html_title:           "Ruby: Päivämäärän muuttaminen merkkijonoksi."
simple_title:         "Päivämäärän muuttaminen merkkijonoksi."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Sinun kannattaa oppia, kuinka muuntaa päivämäärä merkkijonoksi, koska se on tärkeä osa ohjelmointia ja voi auttaa sinua tehostamaan päivämäärätietojen käsittelyä.

## Kuinka tehdä

```Ruby
date = Date.today
puts date.to_s
```

Tämä yksinkertainen koodiesimerkki näyttää, miten voit muuntaa nykyisen päivämäärän merkkijonoksi Rubyssa. Tämä helpottaa päivämäärätietojen tallentamista ja käyttämistä eri ohjelmissa ja algoritmeissa. Tulosteena saadaan nykyinen päivä muodossa "yyyy-mm-dd".

```Ruby
date = Date.new(2020, 12, 31)
puts date.strftime("%d/%m/%Y")
```

Toinen esimerkki käyttää strftime-metodia muokkaamaan päivämäärää eri muotoon. Tässä tapauksessa tulosteena saadaan päivämäärä muodossa "31/12/2020". Voit käyttää erilaisia merkkiyhdistelmiä määrittääksesi haluamasi muodon.

```Ruby
date = Time.new(2021, 1, 1, 12, 30, 0)
puts date.to_s
```

Voit myös muuntaa ajan merkkijonoksi käyttämällä Time-luokan to_s-metodia. Tämä toimii samalla tavalla kuin Date-luokan to_s-metodi ja tulosteena saadaan aika muodossa "hh:mm:ss".

## Syventävä tieto

Päivämäärän muuntaminen merkkijonoksi on tärkeä taito, jota tarvitaan usein ohjelmoinnissa. Se helpottaa päivämäärätietojen käsittelyä ja varmistaa, että ne ovat yhteensopivia käytettyjen algoritmien ja ohjelmien kanssa.

Muuntamisen lisäksi Rubyssa on myös mahdollista luoda uusia päivämääriä käyttäen Date-luokan parse-metodia. Tämä mahdollistaa päivämäärän lukemisen merkkijonosta ja sen muuntamisen Date-objektiksi.

## Katso myös

- [Ruby Date-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [strftime-merkkiyhteenvedot](https://www.techotopia.com/index.php/Ruby_Date_and_Time_-_strftime_Format_Code_Y=%26_g)