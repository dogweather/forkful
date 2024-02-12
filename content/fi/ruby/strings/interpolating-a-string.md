---
title:                "Merkkijonon interpolointi"
aliases:
- fi/ruby/interpolating-a-string.md
date:                  2024-01-20T17:51:28.257230-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Stringin interpolointi tarkoittaa muuttujien tai lausekkeiden arvojen syöttämistä suoraan merkkijonoon. Koodaajat tekevät tätä, koska se tekee koodista selkeämpää ja dynaamisempaa – ei enää yhteenkasattuja merkkijonoja.

## How to (Kuinka tehdä):
```Ruby
# Muuttujan interpolointi
kayttaja = 'Pekka'
tervehdys = "Hei, #{kayttaja}!"
puts tervehdys
# Output: Hei, Pekka!

# Lausekkeen interpolointi
pisteet = 55
viesti = "Sinulla on #{pisteet / 10.0} tähteä!"
puts viesti
# Output: Sinulla on 5.5 tähteä!
```

## Deep Dive (Sukellus syvemmälle):
Stringin interpolointissa #{...} sisällä oleva koodi suoritetaan, ja sen arvo muunnetaan merkkijonoksi. Tämä tapahtui Rubyssa ensi kertaa version 1.8 myötä ja on siitä lähtien ollut suosittu tapa yhdistää tietoa merkkijonoihin.

Vaihtoehtoina ovat `+`-merkin käyttö tai `sprintf`-metodi, mutta ne voivat olla kömpelömpiä ja vähemmän suoraviivaisia. Esimerkiksi:
```Ruby
# Stringin yhdistäminen + merkin avulla
tervehdys = 'Hei, ' + kayttaja + '!'

# sprintf-metodin käyttäminen
tervehdys = sprintf('Hei, %s!', kayttaja)
```

Interpoloinnissa merkkijonoon voi sijoittaa minkä tahansa Ruby-lausekkeen, joka palauttaa arvon. Järjestelmä kutsuu automaattisesti objektin `to_s`-metodia sen muuntamiseksi merkkijonoksi, joten voit interpoloida myös muita kuin merkkijonoja.

## See Also (Katso myös):
- Ruby-dokumentaatio: [String#%](https://ruby-doc.org/core/String.html#method-i-25) kertoo sprintf-metodin käytöstä.
- Ruby-doc.org: [String luku](https://ruby-doc.org/core/String.html), jossa on eritelty muita merkkijonojen käsittelytapoja.
- [Why's (Poignant) Guide to Ruby](http://poignant.guide/), hauska ja informatiivinen opas Ruby-ohjelmoinnin alkeisiin.
