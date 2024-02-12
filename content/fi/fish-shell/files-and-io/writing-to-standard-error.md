---
title:                "Kirjoittaminen standardivirheeseen"
aliases:
- fi/fish-shell/writing-to-standard-error.md
date:                  2024-02-03T19:33:25.109127-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Virheiden kirjoittaminen (stderr) Fish Shellissä tarkoittaa virheilmoitusten tai diagnostiikkatietojen ohjaamista erillään tavallisesta tulosteesta (stdout). Ohjelmoijat tekevät näin varmistaakseen, että virhetietoja voidaan helposti tunnistaa, hallita tai ohjata uudelleen, mikä helpottaa sujuvampaa vianetsintää ja lokien käsittelyä.

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
