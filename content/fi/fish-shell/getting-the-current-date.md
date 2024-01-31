---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:14:07.400111-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Saada nykyinen päivämäärä tarkoittaa hankkia tieto siitä, mikä päivä tänään on. Ohjelmoijat käyttävät sitä tehtävässä ajan merkitsemiseen, lokitukseen ja päivämääräriippuvaisten toimintojen ohjaamiseen.

## How to: (Kuinka tehdä:)
```Fish Shell
# Hae nykyinen päivämäärä oletusmuodossa
set -l current_date (date)
echo $current_date
```
Esimerkkituloste:
```
ti syys 21 12:34:56 EEST 2023
```
```Fish Shell
# Hae nykyinen päivämäärä mukautetussa muodossa
set -l current_date (date "+%Y-%m-%d")
echo $current_date
```
Esimerkkituloste:
```
2023-09-21
```

## Deep Dive (Syväsukellus)
Nykyisen päivämäärän hankkiminen on yleinen operaatio monille ohjelmointikielille, ja se juontaa juurensa aikaisista käyttöjärjestelmistä, jotka ovat tarvinneet käsitellä aikaa. Fish Shellissä `date` komento kutsumalla järjestelmän omaa työkalua voidaan hakea päivämäärä ja aika. Käyttö UNIX-järjestelmissä on pitkälti samankaltaista kuin muissakin kuorissa, kuten Bashissa tai Zsh:ssa. Vaihtoehdot päivämäärän hakemiseen Fishissä ovat limitoidut, sillä se kutsuu suoraan ulkoisia ohjelmia tässä tehtävässä, toisin kuin joissakin kielissä, jotka tarjoavat sisäänrakennetun tavan käsitellä päivämääriä. Fish ei sisällä omaa päivämäärä-funktiota, joten käyttäjä on riippuvainen `date` komennosta.

## See Also (Katso Myös)
- Fish Shellin virallinen dokumentaatio: https://fishshell.com/docs/current/index.html
- `date` komennon manuaalisivu: `man date` komennon avulla tai verkossa https://linux.die.net/man/1/date
- Ajan käsittelyohjeita ohjelmointiin yleisesti: https://en.wikipedia.org/wiki/System_time
