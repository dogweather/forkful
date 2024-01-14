---
title:                "Ruby: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kaksi päivämäärää?

Oletko koskaan halunnut vertailla kahta päivämäärää Rubyssa? Se voi olla hyödyllistä esimerkiksi kun haluat tietää onko jokin tietty päivämäärä ennen vai jälkeen toista. Tässä blogikirjoituksessa opit miten voit vertailla kahta päivämäärää käyttämällä Rubya.

## Miten tehdä se?

Vertaileminen on helppoa käyttäen Rubyn Date-luokkaa. Voit luoda uuden Date-olion käyttäen `Date.parse` metodia ja antamalla sille päivämäärän merkkijonona. Tämän jälkeen voit käyttää `>` ja `<` operaattoreita vertailemiseen.

```Ruby
päivä1 = Date.parse("2020-05-28")
päivä2 = Date.parse("2020-05-29")

if päivä1 > päivä2 
  puts "Päivä 1 on myöhempi."
end

# Output: Päivä 1 on myöhempi.
```

## Syväluotaus

Rubyn Date-luokka tarjoaa myös muita hyödyllisiä metodeja vertailuun, kuten `between?` joka tarkistaa onko jokin päivämäärä kahden annetun päivämäärän välissä. Voit myös käyttää `==` operaattoria tarkistaaksesi ovatko päivämäärät täsmälleen samat.

On myös tärkeää huomata, että Date-oliot eivät sisällä aikatietoja. Jos haluat vertailla myös aikoja, kannattaa käyttää DateTime-luokkaa.

# Katso myös

- [Ruby Date-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Ruby DateTime-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)