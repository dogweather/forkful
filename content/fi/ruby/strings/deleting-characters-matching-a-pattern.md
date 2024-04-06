---
date: 2024-01-20 17:43:12.150351-07:00
description: "How to: Syv\xE4sukellus: Merkkien poistaminen kuvion mukaan Rubyss\xE4\
  \ perustuu s\xE4\xE4nn\xF6llisiin lausekkeisiin (regular expressions), jotka tulivat\
  \ mukaan\u2026"
lastmod: '2024-04-05T22:38:57.685269-06:00'
model: gpt-4-1106-preview
summary: "Syv\xE4sukellus: Merkkien poistaminen kuvion mukaan Rubyss\xE4 perustuu\
  \ s\xE4\xE4nn\xF6llisiin lausekkeisiin (regular expressions), jotka tulivat mukaan\
  \ ohjelmointikieleen jo varhaisessa vaiheessa. `gsub`-metodi on tehokas ty\xF6kalu\
  \ tekstik\xE4sittelyyn, ja se korvaa kaikki l\xF6ydetyt kuviot m\xE4\xE4ritt\xE4\
  m\xE4si korvaajan kanssa. Vaihtoehtoina voit k\xE4ytt\xE4\xE4 `delete`-metodia,\
  \ joka on yksinkertaisempi ja nopeampi, mutta v\xE4hemm\xE4n joustava, koska se\
  \ ei salli kuvioiden k\xE4ytt\xF6\xE4. Historiallisesti, tekstik\xE4sittely on ollut\
  \ kiinte\xE4 osa ohjelmointia ja Rubyss\xE4 tekstitietojen k\xE4sittelyn mekanismit\
  \ ovat erityisen voimakkaita. T\xE4st\xE4 syyst\xE4, Ruby on suosittu skriptikieli\
  \ monissa datan k\xE4sittely\xE4 ja web-kehityst\xE4 vaativissa projekteissa."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

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
