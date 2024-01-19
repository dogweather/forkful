---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# Rubyssa merkkijonojen alijonojen poimiminen

## Mikä & Miksi?

Substringien poimiminen tarkoittaa sitä, että valitsemme osan merkkijonosta. Ohjelmoijat tekevät tämän, koska usein tarvitsemme vain osan tiedoista, joita merkkijono sisältää.

## Näin teet:

```Ruby
str = "Hei maailma"
puts str[0,3]    # "Hei"
puts str[4,6]    # "maailma"
puts str[-1]     # "a"
```

Koodissa alijonojen indeksit aloitetaan merkkijonon alusta. Negatiiviset indeksit alkavat merkkijonon lopusta.

```Ruby
str = "rubyonrails"
puts str["ruby"] # "ruby"
puts str["lua"]  # nil
```

Näissä esimerkeissä käytetty metodi palauttaa alijonon, jos se löytyy merkkijonosta. Muuten palautetaan nil.

## Syvemmälle

Historiassa Rubyn alkuperäiset tekijät lisäsivät substring-toiminnot, koska ne ovat välttämättömiä monissa ohjelmointitehtävissä. Ruby tarjoaa useita tapoja tehdä tämä, kuten `slice()`, `[]`, `substring()`. Todellinen toteutus riippuu siitä, mitä tarvitaan ja kuinka monimutkainen merkkijono on.

## Katso myös:

Linkit muihin lähteisiin, jotka voivat olla hyödyllisiä:

1. Rubyn virallinen dokumentaatio merkkijonojen alijonoista: https://ruby-doc.org/core-2.7.1/String.html
2. Verkkokurssi Ruby-merkkijonoista: https://www.rubyguides.com/2015/03/ruby-strings/
3. StackOverflow-keskustelut Rubyn alijonoista: https://stackoverflow.com/questions/tagged/ruby+substring