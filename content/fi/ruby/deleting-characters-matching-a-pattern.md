---
title:                "Hahmonpoisto vastaavaa kuvioita vasten"
html_title:           "Ruby: Hahmonpoisto vastaavaa kuvioita vasten"
simple_title:         "Hahmonpoisto vastaavaa kuvioita vasten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Tämä voi olla hyödyllistä, kun käsittelet tekstimuotoisia tietoja, kuten CSV- tai JSON-tiedostoja, ja haluat jättää pois tietyt merkit, jotka eivät ole tarpeellisia.

## Kuinka tehdä

Poistaaksesi merkit, jotka vastaavat tiettyä kaavaa, voit käyttää `.gsub` -menetelmää. Tämä menetelmä korvaa kaikki merkit, jotka vastaavat määritettyä kaavaa, tyhjällä merkillä. Tässä on esimerkki:

```Ruby
text = "Tämä on esimerkkiteksti (123)."

puts text.gsub(/[()]/, '')
```

Tämä tuottaa seuraavan tulosteen:

`Tämä on esimerkkiteksti 123.`

Kuten näet, sulkumerkit on korvattu tyhjällä merkillä ja tekstissä oleva kaikki muu on säilytetty.

## Syvemmälle

Voit käyttää erilaisia RegExp-kaavoja poistaaksesi erilaisia merkkejä. Esimerkiksi voit käyttää `\d` vastaamaan numeroita tai `\w` vastaamaan kirjaimia ja numeroita. Voit myös käyttää `+` -merkkiä vastaamaan useita toistuvia merkkejä. Esimerkiksi kaava `/[()]+/` poistaisi kaikki sulkumerkit, mutta myös välilyönnit, mikäli niitä olisi. Voit kokeilla erilaisia RegExp-kaavoja ja nähdä, miten ne vaikuttavat tekstiin.

## Katso myös

- Ruby String -dokumentaatio: https://ruby-doc.org/core-3.0.2/String.html
- Säännölliset lausekkeet Rubyn String -esimerkkejä: https://www.rubyguides.com/2019/02/ruby-regex/
- Ruby String `gsub` -menetelmästä: https://www.rubyguides.com/ruby-tutorial/ruby-string-gsub-method/