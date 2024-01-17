---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Ruby: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

Mikä on merkkijonon pituuden selvittäminen? Se on yksinkertainen tapa mitata merkkijonon merkkien määrää, eli kuinka monta merkkiä merkkijono sisältää. Ohjelmoijat tekevät tätä usein, sillä merkkijonojen pituuden selvittäminen auttaa heitä käsittelemään ja analysoimaan tekstidataa.

# Miten:

```ruby
string = "Tämä on esimerkki merkkijonosta"

puts string.length
# Output: 30
```
Tässä yksinkertaisessa koodiesimerkissä luodaan muuttuja nimeltä `string` ja tallennetaan siihen tietty merkkijono. Sitten käytetään `length`-metodia, joka laskee kuinka monta merkkiä merkkijonossa on ja tulostaa sen konsoliin. Tässä tapauksessa merkkijonossa oli 30 merkkiä, minkä takia tulostettu luku on myös 30.

# Syvempi sukellus:

Merkkijonojen pituuden selvittämisen historia juontaa juurensa takaisin vanhoihin tietokoneisiin, joissa se oli tärkeä osa tekstipohjaisten käyttöliittymien toimintaa. Nykyäänkin se on edelleen tärkeä osa ohjelmointia, sillä usein tarvitaan tietoa siitä, kuinka pitkä esimerkiksi käyttäjän antama syöte on.

Merkkijonojen pituuden selvitettämisellä on myös useita vaihtoehtoisia tapoja, kuten `size`- ja `count`-metodit, jotka toimivat samalla periaatteella. Lisäksi monet ohjelmointikielillä sisältävät valmiita funktioita merkkijonojen pituuden selvittämiseen.

# Katso myös:

- [Ruby String Documentation](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby API - length](https://rubyapi.org/3.0/o/string#method-i-length)
- [Difference between size and length in Ruby](https://dev.to/rpalo/ruby-quirk-size-vs-length-9lo)