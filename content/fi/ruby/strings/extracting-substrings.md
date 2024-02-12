---
title:                "Merkkijonojen osien poimiminen"
aliases:
- /fi/ruby/extracting-substrings.md
date:                  2024-01-20T17:46:41.547380-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Substringien poiminta tarkoittaa isomman merkkijonon sisältä tietyn osan irrottamista. Ohjelmoijat tekevät tätä, koska usein tarvitaan vain jonkin tiedon palanen, esimerkiksi käyttäjänimi tai tiedostopääte.

## How to: (Kuinka tehdään:)
```Ruby
# Oletusmerkkijono
merkkijono = "HeiMaailma"

# indeksien mukaan (alku, pituus)
substringi = merkkijono[3, 5]
puts substringi  # Maail

# kuviohaun mukaan
tunniste_kuvio = /aail/
substringi = merkkijono[tunniste_kuvio]
puts substringi  # aail

# kuvion ja indeksien yhdistelmän mukaan
substringi = merkkijono[/aail(.)/, 1]
puts substringi  # m

# alku- ja loppuindeksit (range)
substringi = merkkijono[1..4]
puts substringi  # eiMa
```

## Deep Dive (Sukellus syvemmälle):
Substringien poiminta Rubyssä on helppoa ja joustavaa, ja se on ollut kielen ominaisuus jo ensiversioista lähtien. Historiallisesti, Ruby on inspiroitunut monista aiemmista kielistä, kuten Perlista, jossa merkkijonojen käsittelyä on aina pidetty tärkeänä.
Vaihtoehtoisia tapoja poimia osajonoja Rubyssa ovat `slice` ja `slice!` metodit, jotka toimivat samankaltaisesti kuin hakasulkumerkintä, mutta voivat myös muuttaa alkuperäistä merkkijonoa.
Suorituskyvyn näkökulmasta Ruby käsittelee merkkijonoja omana tietorakenteenaan, mutta jos merkkijonoja manipuloidaan paljon, suorituskyky voi heikentyä suurilla datamäärillä, koska jokainen operaatio luo uuden merkkijonon. 

## See Also (Katso myös):
- Ruby-dokumentaatio merkkijonoista: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- Stack Overflow -keskusteluja ja esimerkkejä merkkijonojen käsittelystä Rubyssa: [https://stackoverflow.com/questions/tagged/ruby+string](https://stackoverflow.com/questions/tagged/ruby+string)
- Interaktiivisen koodikokeilun tarjoava sivusto: [https://repl.it/languages/ruby](https://repl.it/languages/ruby)
