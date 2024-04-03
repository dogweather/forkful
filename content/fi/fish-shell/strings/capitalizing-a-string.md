---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:34.866104-07:00
description: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa sen muokkaamista\
  \ siten, ett\xE4 ensimm\xE4inen kirjain on isolla ja loput merkkijonosta ovat pienell\xE4\
  . T\xE4m\xE4\u2026"
lastmod: '2024-03-13T22:44:56.975206-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa sen muokkaamista siten,\
  \ ett\xE4 ensimm\xE4inen kirjain on isolla ja loput merkkijonosta ovat pienell\xE4\
  ."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

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
