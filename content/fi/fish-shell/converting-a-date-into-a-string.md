---
title:                "Päivämäärän muuntaminen merkkijonoksi"
date:                  2024-01-20T17:36:19.845274-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"

category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
Muuntaminen päivämäärästä merkkijonoksi tarkoittaa päivämäärän esittämistä luettavassa muodossa. Ohjelmoijat tekevät tämän, jotta päivämäärät olisivat ihmisen ymmärrettävissä ja sopivat eri järjestelmiin.

## How to: - Kuinka:
```Fish Shell
# Muunnetaan nykyinen päivämäärä merkkijonoksi
set päivämäärä (date "+%Y-%m-%d %H:%M:%S")
echo $päivämäärä
```
Esimerkkitulostus:
```
2023-04-05 14:30:25
```

```Fish Shell
# Määritellään mukautettu muoto
set mukautettu_päivämäärä (date "+%d.%m.%Y")
echo $mukautettu_päivämäärä
```
Esimerkkitulostus:
```
05.04.2023
```

## Deep Dive - Syväsukellus
Fish Shellissä päivämäärän muuntaminen merkkijonoksi hyödyntää `date`-komentoa, joka on ollut Unix-järjestelmissä vuosikymmeniä. Vaihtoehtoina on käyttää erilaisia päivämääräkirjastoja eri ohjelmointikielillä. Fishissä oleellista on yksinkertaisuus ja selkeys, siksi `date` on yleisin työkalu.

Muotomääreet, kuten `%Y` vuodelle tai `%d` päivälle, määrittävät tulosteen muodon. Ne ovat standardoituja ja laajalti tuettuja.

## See Also - Katso Myös
- Fish Shellin kotisivu: [https://fishshell.com](https://fishshell.com)
- `date`-komento: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Unix Time Muunnin: [https://www.unixtimestamp.com/](https://www.unixtimestamp.com/)
