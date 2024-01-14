---
title:    "Fish Shell: Kahden päivämäärän vertailu"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi
Haluatko tarkastella kahden päivämäärän eroa Fish Shell -ohjelmoinnin avulla? Tässä blogikirjoituksessa kerron, miten se on mahdollista ja kuinka syventyä tähän aiheeseen.

## Kuinka tehdä
Fish Shell -ohjelmoinnissa voit käyttää "date" -komentoa vertaamaan kahta päivämäärää. Käytä "date" -komentoa muodossa "date -d 'YYYY-MM-DD'". Esimerkiksi, haluat verrata kahta päivämäärää 2021-01-01 ja 2021-01-05. Kirjoita seuraava koodi Fish Shell -tiedostoon:

```Fish Shell
date -d '2021-01-05' -d '2021-01-01'
```

Tämä tulostaa päivien määrän kahden päivämäärän välillä:

```Fish Shell
4
```

## Syvempää tietoa
"date" -komento käyttää UNIX-timestampia vertailemaan päivämääriä. UNIX-timestamp tarkoittaa sekuntien määrää, jotka ovat kuluneet 1. tammikuuta 1970 klo 00:00 UTC:sta kyseiseen päivämäärään. Tämä tarkoittaa sitä, että esimerkin tulostama "4" tarkoittaa 4 päivää, jotka ovat kuluneet 1. tammikuuta 1970 ja 1. tammikuuta 1970 välillä.

## Katso myös
- [Fish Shell -dokumentaatio](https://fishshell.com/docs/current/cmds/date.html)
- [Blogikirjoitus: Kuinka vertailla aikoja Fish Shell -ohjelmoinnissa (englanniksi)](https://example.com/blog/comparing-dates-fish-shell)
- [GitHub -sivusto Fish Shell -käyttäjille (englanniksi)](https://github.com/fish-shell/fish-shell)