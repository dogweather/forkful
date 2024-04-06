---
date: 2024-01-20 17:36:19.845274-07:00
description: 'How to: - Kuinka: Esimerkkitulostus.'
lastmod: '2024-04-05T22:38:57.618307-06:00'
model: gpt-4-1106-preview
summary: '- Kuinka: Esimerkkitulostus.'
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

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
