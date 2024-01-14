---
title:                "Ruby: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Voit poistaa merkkejä jotka vastaavat tiettyä kaavaa kun haluat muokata tai siistiä tekstiä.

## Kuinka tehdä

```ruby
# Luo uusi merkkijono
string = "Tämä on esimerkki tekstistä, jossa on turhia välilyöntejä"

# Käytä regular expression (säännöllinen lauseke) löytääksesi välilyönnit ja korvaa ne tyhjillä merkeillä
new_string = string.gsub(/\s+/, "")

# Tulostaa uuden merkkijonon ilman välilyöntejä
puts new_string
```
Tulos:
"Tämäonesimerkkitekstistä,jossaturhiaäälyöntejä"

## Syvempi sukellus

> Regular expressions (myös nimellä regex) ovat erityinen merkkijonojen käsittelytyökalu, jota käytetään löytämään ja korvaamaan merkkejä tai sanoja tiettyjen kaavojen mukaisesti.

Regular expressions voivat olla hyödyllisiä tekstinkäsittelyssä, kun haluat suorittaa tarkkoja muokkauksia. Voit esimerkiksi käyttää niitä poistaaksesi ylimääräisiä välilyöntejä, kuten yllä olevassa esimerkissä, tai löytää ja korvata tiettyjä sanoja tai merkkijonoja.

Regular expressionsin avulla voit myös määrittää monimutkaisempia kaavoja, kuten etsiä merkkijonoja, jotka alkavat tai päättyvät tietyillä kirjaimilla, tai joilla on tietty määrä merkkejä välissä.

## Katso myös

- ["The Ruby Regular Expression Cheat Sheet" (Ruby säännöllisten lausekkeiden huijausarkki)](https://www.rubyguides.com/2015/06/ruby-regular-expressions/)
- ["Ruby Regex: The Ultimate Beginner's Guide" (Ruby säännölliset lausekkeet: lopullinen aloittelijan opas)](https://yukimasano.medium.com/ruby-regex-the-ultimate-beginners-guide-228a5674d0e4)
- ["Ruby Regular Expressions Tutorial: How to Get Started" (Ruby säännöllisten lausekkeiden opas: miten aloittaa)](https://www.rubyguides.com/2015/06/ruby-regular-expression/)