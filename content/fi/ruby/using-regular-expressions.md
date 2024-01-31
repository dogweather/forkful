---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Säännölliset lausekkeet eli regexit etsivät ja manipuloivat tekstiä. Ne säästävät aikaa ja riviä tekemällä monimutkaiset tekstioperatiot yksinkertaisiksi.

## How to:
```Ruby
# Etsitään sanoja, jotka alkavat 't':llä
teksti = "Tänään on torstai, tosi taianomainen!"
regex = /\bt\w*/
sanat = teksti.scan(regex)
puts sanat  # Output: ["Tänään", "torstai", "tosi", "taianomainen"]

# Korvataan kaikki numerot tähdillä
numeroteksti = "A123 on koodi, 456B on toinen."
korvattu_teksti = numeroteksti.gsub(/\d/, '*')
puts korvattu_teksti  # Output: "A*** on koodi, ***B on toinen."
```

## Deep Dive
Regexit ovat olleet olemassa 1950-luvulta lähtien, ja niiden juuret ulottuvat automaattiteoriaan. Rubyssa regexit ovat ensiluokkaisia kansalaisia, mikä tarkoittaa, että niitä tuetaan kielen syntaksissa nativiivisti. Vaihtoehtoina voit käyttää merkkijonojen metodeja, kuten `include?`, `start_with?` tai `end_with?`, mutta ne eivät tarjoa regexien joustavuutta. Rubyssa regexit toteutetaan `Regexp`-luokalla, ja ne hyötyvät tehokkaasta Onigmo-moottorista, joka tukee monimutkaista pattern matchingiä.

## See Also
- Ruby-doc: [Regexp](https://ruby-doc.org/core-2.7.0/Regexp.html)
- "Programming Ruby: The Pragmatic Programmer's Guide": [Regular Expressions](http://ruby-doc.com/docs/ProgrammingRuby/)
- Rubular: [Ruby Regular Expression Editor](http://rubular.com/)
