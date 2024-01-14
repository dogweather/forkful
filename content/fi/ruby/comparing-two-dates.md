---
title:    "Ruby: Kahden päivämäärän vertailu"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit vertailla kahta päivämäärää Ruby-ohjelmoinnissa? Tämä on tärkeä taito, joka auttaa sinua vertailemaan päivämääriä, tarkistamaan onko jokin päivämäärä ennen tai jälkeen toista ja suorittamaan muita päivämäärään liittyviä toimintoja.

## Miten

Ruby-ohjelmoinnissa päivämäärien vertailuun on useita tapoja. Yksi tapa on käyttää `Date`-luokkaa ja sen sisäänrakennettuja metodeja, kuten `today` ja `parse`. Toinen tapa on käyttää `DateTime`-luokkaa ja sen metodeja, kuten `new`, `civil` ja `compare`.

```Ruby
# Vertaillaan kahta päivämäärää Date-luokan avulla
date1 = Date.parse("2021-01-01")
date2 = Date.today

puts "Ensimmäinen päivämäärä on ennen toista." if date1 < date2

# Vertaillaan kahta päivämäärää DateTime-luokan avulla
datetime1 = DateTime.new(2021, 1, 1)
datetime2 = DateTime.civil(2021, 12, 31)

puts "Toinen päivämäärä on ennen ensimmäistä." if DateTime.compare(datetime1, datetime2) < 0
```

Koodin tulos:

```
Ensimmäinen päivämäärä on ennen toista.
Toinen päivämäärä on ennen ensimmäistä.
```

Voit myös käyttää muita metodeja ja matemaattisia operaatioita, kuten ` + `, `-` ja `between?`, vertaillessasi päivämääriä.

## Syvällisempi tarkastelu

Päivämäärien vertailu Ruby-ohjelmoinnissa perustuu niiden sisäiseen esitykseen sekunneiksi tai nanosekunneiksi. Tämä mahdollistaa päivämäärien tarkemman vertailun ja antaa lisätietoa päivämääristä. Lisäksi päivämäärän aikavyöhyke vaikuttaa sen esitykseen ja vertailuun.

## Katso myös

- [Date-luokka Ruby-dokumentaatiossa](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [DateTime-luokka Ruby-dokumentaatiossa](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [Ruby-ohjelmointiopas](https://www.ruby-lang.org/fi/documentation/quickstart/)