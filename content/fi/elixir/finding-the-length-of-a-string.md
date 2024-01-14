---
title:                "Elixir: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet aloittelemassa Elixir-ohjelmointia tai haluat oppia uutta kielen ominaisuuksista, yksi hyödyllisimmistä asioista, joita voit tehdä, on oppia löytämään merkkijonon pituus. Tämä taito on välttämätön monissa ohjelmointitehtävissä, ja sen oppiminen auttaa sinua ymmärtämään paremmin Elixirin toimintatapaa.

## Kuinka

 ```Elixir
string = "Hei, tämä on esimerkkilause."
length = String.length(string)
IO.puts "Merkkijonon pituus on: #{length}"
```

Tässä esimerkissä luomme muuttujan nimeltä "string" ja annamme sille merkkijonon arvon. Sitten käytämme "String.length" -funktiota löytääksemme merkkijonon pituuden. Viimeisessä rivillä tulostamme pituuden käyttäen "IO.puts" -funktiota. Tämän koodin suorittamisen pitäisi tulostaa seuraava viesti: "Merkkijonon pituus on: 27".

## Syvempää sukellusta

Merkkijonojen käsittely on tärkeä osa Elixir-ohjelmointia, ja löytämällä merkkijonon pituuden voit tehdä useita muita toimintoja, kuten esimerkiksi leikata tai yhdistää merkkijonoja. Tämä tapahtuu osittain siksi, että Elixir käsittelee merkkijonoja tavalla, joka poikkeaa monista muista kielistä. Esimerkiksi merkkijonoja ei tallenneta taulukoissa, mikä tarkoittaa, että ne eivät voi olla "yllekkäisiä" eli sisältää toisia merkkijonoja sisällään.

Lisäksi, Elixirin "String" -moduuli tarjoaa paljon muitakin hyödyllisiä toimintoja merkkijonojen käsittelyyn. Kannattaa tutustua näihin toimintoihin lisää oppiaksesi lisää tästä aiheesta.

## Katso myös

- [Elixi Tutorial – Merkkijonot](https://elixirschool.com/fi/lessons/basics/strings/)
- [Totuudet ja tehtävinen – Merkkijonot](https://www.learnelixir.tv/s/string-basics/)
- [Merkkijonot Elixirin virallisella dokumentaatiossa](https://hexdocs.pm/elixir/String.html)