---
aliases:
- /fi/bash/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:55.293122-07:00
description: "Bash-ohjelmoinnissa tarkistus, onko hakemisto olemassa, on olennainen\
  \ valvontamekanismi, jota k\xE4ytet\xE4\xE4n varmistamaan hakemiston olemassaolo\
  \ ennen\u2026"
lastmod: 2024-02-18 23:09:07.820255
model: gpt-4-0125-preview
summary: "Bash-ohjelmoinnissa tarkistus, onko hakemisto olemassa, on olennainen valvontamekanismi,\
  \ jota k\xE4ytet\xE4\xE4n varmistamaan hakemiston olemassaolo ennen\u2026"
title: Tarkistetaan, onko hakemisto olemassa
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Bash-ohjelmoinnissa tarkistus, onko hakemisto olemassa, on olennainen valvontamekanismi, jota käytetään varmistamaan hakemiston olemassaolo ennen tiedosto-operaatioiden suorittamista. Tämä tarkistus on ratkaiseva välttääkseen virheet, kuten yrittämisen päästä käsiksi tai muokata olemattomia hakemistoja, mikä varmistaa sujuvamman ja ennakoitavamman skriptin suorituksen.

## Kuinka:

Perustasolla Bash mahdollistaa hakemiston olemassaolon tarkistamisen käyttämällä ehtolauseita ja `-d` operaattoria. Alla on suoraviivainen esimerkki, joka osoittaa, miten tämä tarkistus suoritetaan.

```bash
if [ -d "/polku/hakemistoon" ]; then
    echo "Hakemisto on olemassa."
else
    echo "Hakemistoa ei ole olemassa."
fi
```

Esimerkkituloste (jos hakemisto on olemassa):
```
Hakemisto on olemassa.
```

Esimerkkituloste (jos hakemistoa ei ole olemassa):
```
Hakemistoa ei ole olemassa.
```

Monimutkaisemmissa skripteissä on yleistä yhdistää tarkistus muihin toimiin, kuten luoda hakemisto, jos sitä ei ole olemassa:

```bash
DIR="/polku/hakemistoon"
if [ -d "$DIR" ]; then
    echo "$DIR on olemassa."
else
    echo "$DIR ei ole olemassa. Luodaan nyt..."
    mkdir -p "$DIR"
    echo "$DIR luotu."
fi
```

Esimerkkituloste (jos hakemistoa ei ole olemassa ja sitten se luodaan):
```
/polku/hakemistoon ei ole olemassa. Luodaan nyt...
/polku/hakemistoon luotu.
```

Vaikka Bash itsessään tarjoaa vankkoja työkaluja tällaisiin tarkistuksiin, ei suosittuja kolmannen osapuolen kirjastoja erityisesti tähän tehtävään ole, sillä natiivit Bash-komennot ovat täysin kykeneviä ja tehokkaita hakemiston olemassaolon varmentamisessa.
