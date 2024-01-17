---
title:                "Alimerkkijonojen erottaminen"
html_title:           "Ruby: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Substringien erottaminen on prosessi, jossa ohjelmoijat etsivät ja poimivat osia merkkijonoista, jotka haluavat manipuloida tai tarkastella tietyn algoritmin tai funktion suorittamiseksi. Tätä käytetään usein muutosten tekemiseen olemassa oleviin merkkijonoihin tai niiden jakamiseen pienempiin osiin datan käsittelyä varten.

# Miten tehdä se:
Erota alamerkkijonoja Rubyssa voidaan tehdä useilla eri tavoilla, mutta yleisin tapa on käyttää String-luokan sisäänrakennettuja metodeita, kuten #slice ja #substring. Seuraavassa esimerkissä näytämme, miten voit erottaa alamerkkijonon tietystä merkkijonosta käyttämällä #slice metodia:

```ruby
string = "Tervetuloa Rubyyn!"
puts string.slice(0, 9)
```
Tämä tulostaisi "Tervetulo" terminaalissa. Ensimmäinen parametri #slice metodissa on aloituskirjain indeksi, ja toinen on lopetuskirjain indeksi.

# Syvemmälle:
Substringien erottaminen on ollut tärkeä osa ohjelmointia pitkään. Alun perin kehitsi IBM:n kehittäjä, ohjelmointikieltä nimeltä SNOBOL. Kuten aiemmin mainittu, Rubyssa on erilaisia ​​tapoja erottaa alamerkkijonot, kuten myös #[] -metodilla, jota käytetään pääasiassa lopputaulukoinnissa.

## Katso myös:
- Ruby String-luokan dokumentaatio: https://ruby-doc.org/core-2.7.1/String.html
- Ruby String-luokan Github-lähdekoodi: https://github.com/ruby/ruby/blob/master/string.c
- String methods in Ruby – SitePoint article: https://www.sitepoint.com/ruby-string-methods/