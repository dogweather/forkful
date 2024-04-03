---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:25.109127-07:00
description: "Virheiden kirjoittaminen (stderr) Fish Shelliss\xE4 tarkoittaa virheilmoitusten\
  \ tai diagnostiikkatietojen ohjaamista erill\xE4\xE4n tavallisesta tulosteesta\u2026"
lastmod: '2024-03-13T22:44:57.011542-06:00'
model: gpt-4-0125-preview
summary: "Virheiden kirjoittaminen (stderr) Fish Shelliss\xE4 tarkoittaa virheilmoitusten\
  \ tai diagnostiikkatietojen ohjaamista erill\xE4\xE4n tavallisesta tulosteesta (stdout)."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Miten:
Fish Shellissä voit kirjoittaa stderr-näkymään ohjaamalla tulosteesi käyttäen `>&2`. Tässä on perusesimerkki:

```fish
echo "Tämä on virheilmoitus" >&2
```

Tämä komento yksinkertaisesti kaikuttaa viestin stderr-näkymään stdoutin sijaan. Jos kirjoittaisit skriptin, joka tuottaa sekä tavallisia että virheviestejä, saattaisit tehdä jotakin tällaista:

```fish
echo "Prosessin aloitus"
echo "Tapahtui virhe" >&2
echo "Prosessi valmis"
```

Esimerkkitulostus, jos ajat skriptin ja ohjaat stderrin tiedostoon:

```
Prosessin aloitus
Prosessi valmis
```

Virheviesti ei näkyisi tavallisessa tulosteessa, vaan löytyisi tiedostosta, johon ohjasit stderrin.

Tilanteissa, jotka vaativat monimutkaisempaa virheenkäsittelyä tai lokitusta, Fish ei sisällä nimenomaisesti tähän suunniteltuja sisäänrakennettuja kirjastoja. Voit kuitenkin hyödyntää ulkoisia työkaluja tai kirjoittaa funktioita avuksi. Esimerkiksi yksinkertaisen lokitusfunktion luominen voisi näyttää tältä:

```fish
function log_error
    echo $argv >&2
end

log_error "Tämä on edistyksellinen virheviesti"
```

Tämä funktio `log_error` ottaa vastaan minkä tahansa merkkijonon, jonka annat sille, ja kirjoittaa sen stderr-näkymään. Tällaisten funktioiden käyttäminen voi auttaa pitämään virheenkäsittelysi selkeänä ja johdonmukaisena läpi skriptiesi.
