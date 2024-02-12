---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
aliases: - /fi/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:05:34.866104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon alkukirjaimen suurentaminen tarkoittaa sen muokkaamista siten, että ensimmäinen kirjain on isolla ja loput merkkijonosta ovat pienellä. Tämä on yleinen tehtävä tekstinkäsittelyssä, käyttäjäsyötteen normalisoinnissa ja datan muotoilussa varmistamaan yhdenmukaisuus tai tiettyjen muotoilukriteerien täyttäminen.

## Miten:

Fish Shellissä merkkijonoja voidaan käsitellä suoraan sisäänrakennetuilla funktioilla ilman ulkopuolisten työkalujen tai kirjastojen tarvetta. Merkkijonon alkukirjaimen suurentamiseksi voit yhdistää `string`-komennon alikomentoihin.

```fish
# Esimerkkimerkkijono
set sample_string "hello world"

# Suurenna ensimmäinen kirjain
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Tulos:
```
Hello world
```

Skenaarioita varten, jotka vaativat useamman sanan alkukirjaimen suurentamista merkkijonossa (esim. muuttaen "hello world" muotoon "Hello World"), iteroidaan jokaisen sanan yli soveltaen pääomituslogiikkaa kuhunkin:

```fish
# Esimerkkilause
set sentence "hello fish shell programming"

# Suurenna kunkin sanan alkukirjain
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Yhdistä pääomitetut sanat
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Tulos:
```
Hello Fish Shell Programming
```

Huomaa, että Fish Shell ei suoraan tarjoa yksittäisen komennon lähestymistapaa koko lauseen pääomittamiseen samaan tapaan kuin jotkut ohjelmointikielet tekevät merkkijonomenetelmillään. Siksi `string split`, `string sub`, `string upper` yhdistäminen ja sen jälkeen uudelleen yhdistäminen edustaa idiomaattista lähestymistapaa Fish Shellissä tämän saavuttamiseksi.
