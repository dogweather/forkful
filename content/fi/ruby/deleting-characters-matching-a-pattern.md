---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
aliases:
- fi/ruby/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:12.150351-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? 
Mikä & Miksi?

Kun puhutaan merkkien poistamisesta kuvion mukaan Rubyssä, tarkoitamme prosessia, jossa etsitään ja poistetaan merkkejä merkkijonosta tietyllä säännönmukaisuudella. Ohjelmoijat tekevät tämän, jotta voidaan puhdistaa dataa tai muokata tekstiä tiettyjen vaatimusten mukaiseksi.

## How to:
Miten:

```Ruby
# Poistetaan kaikki numerot merkkijonosta
str = "Ruby versio 3.1.0 on uusin!"
clean_str = str.gsub(/[0-9]/, '')
puts clean_str
# Output: "Ruby versio . on uusin!"

# Poistetaan kaikki välimerkit
str = "Hei, maailma! Tervetuloa Rubyyn."
clean_str = str.gsub(/[\.,!]/, '')
puts clean_str
# Output: "Hei maailma Tervetuloa Rubyyn"

# Käytetään `delete`-metodia vokaalien poistoon
str = "Mennään syömään!"
clean_str = str.delete('aeiouyäö')
puts clean_str
# Output: "Mnnn symn!"
```

## Deep Dive
Syväsukellus:

Merkkien poistaminen kuvion mukaan Rubyssä perustuu säännöllisiin lausekkeisiin (regular expressions), jotka tulivat mukaan ohjelmointikieleen jo varhaisessa vaiheessa. `gsub`-metodi on tehokas työkalu tekstikäsittelyyn, ja se korvaa kaikki löydetyt kuviot määrittämäsi korvaajan kanssa. Vaihtoehtoina voit käyttää `delete`-metodia, joka on yksinkertaisempi ja nopeampi, mutta vähemmän joustava, koska se ei salli kuvioiden käyttöä.

Historiallisesti, tekstikäsittely on ollut kiinteä osa ohjelmointia ja Rubyssä tekstitietojen käsittelyn mekanismit ovat erityisen voimakkaita. Tästä syystä, Ruby on suosittu skriptikieli monissa datan käsittelyä ja web-kehitystä vaativissa projekteissa.

## See Also
Katso Myös:

- [Ruby Regular Expressions (Englanniksi)](https://ruby-doc.org/core-3.1.0/Regexp.html)
- [Ruby String#delete (Englanniksi)](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- [Ruby String#gsub (Englanniksi)](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
