---
date: 2024-01-20 17:42:26.009400-07:00
description: "How to: - Miten: K\xE4yt\xE4 `string` komentoa kuvioita vastaavien merkkien\
  \ poistoon. T\xE4ss\xE4 esimerkki, jossa poistetaan kaikki pisteet merkkijonosta."
lastmod: '2024-04-05T22:38:57.587414-06:00'
model: gpt-4-1106-preview
summary: "- Miten: K\xE4yt\xE4 `string` komentoa kuvioita vastaavien merkkien poistoon.\
  \ T\xE4ss\xE4 esimerkki, jossa poistetaan kaikki pisteet merkkijonosta."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: - Miten:
Käytä `string` komentoa kuvioita vastaavien merkkien poistoon. Tässä esimerkki, jossa poistetaan kaikki pisteet merkkijonosta:

```Fish Shell
set sentence "Pisteitä.on.tässä.lauseessa"
echo $sentence | string replace -a '.' ''
```

Tulostus:
```
Pisteitäontässälauseessa
```

Voit myös poistaa useita eri merkkejä käyttämällä regex-syntaksia:

```Fish Shell
set filename "tiedosto_nimi_v1.0.0.txt"
echo $filename | string replace -ra '[._]' ''
```

Tulostus:
```
tiedostonimiv100txt
```

## Deep Dive - Syväsukellus
Fish Shell käyttää `string`-työkalua merkkijonon käsittelyyn, mikä tuli mukaan versiossa 2.3.0 ja korvasi monia aiempia, epäjohdonmukaisia tapoja käsitellä merkkijonoja. Se on suoraviivaisempi kuin perinteiset POSIX-työkalut kuten `sed` tai `awk`, koska se on integroitu suoraan kääntäjään.

Vaihtoehtoja `string`-komennolle ovat `sed` ja `awk`, jotka ovat tehokkaita mutta vaativat enemmän oppimista ja ovat vähemmän selkeästi integroituja Fish-komentotulkin kanssa. Fish Shellin `string`-komennon etu on sen selkeys ja helppokäyttöisyys; ei tarvetta monimutkaisille putkille tai väliaikaistiedostoille.

Suorituskyvyn osalta `string` on yleensä riittävän nopea useimmissa skriptikäyttötapauksissa, vaikkakin `sed` ja `awk` voivat olla nopeampia suurilla datamäärillä. Fish Shell tarjoaa käyttäjäystävällisemmän lähestymistavan, jonka monet arvostavat.

## See Also - Katso Myös
- Fish Shellin `string` ohjeet: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Regex-pohjainen tekstin käsittely: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Vertailu traditionalisen shell-ohjelmoinnin ja Fish Shellin välillä: [https://fishshell.com/docs/current/tutorial.html#tut_unix](https://fishshell.com/docs/current/tutorial.html#tut_unix)
