---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:56.092973-07:00
description: "Kuinka: Kirjoittaaksesi tekstitiedostoon Fishiss\xE4, voit k\xE4ytt\xE4\
  \xE4 `echo`-komentoa yhdess\xE4 uudelleenohjausoperaattoreiden kanssa. Fishille\
  \ ei ole suosittuja\u2026"
lastmod: '2024-03-13T22:44:57.013443-06:00'
model: gpt-4-0125-preview
summary: "Kirjoittaaksesi tekstitiedostoon Fishiss\xE4, voit k\xE4ytt\xE4\xE4 `echo`-komentoa\
  \ yhdess\xE4 uudelleenohjausoperaattoreiden kanssa."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Kuinka:
Kirjoittaaksesi tekstitiedostoon Fishissä, voit käyttää `echo`-komentoa yhdessä uudelleenohjausoperaattoreiden kanssa. Fishille ei ole suosittuja kolmannen osapuolen kirjastoja tiedoston kirjoittamiseen, sillä shellin sisäänrakennetut komennot ovat suoraviivaisia ja tehokkaita tähän tarkoitukseen.

### Tekstin kirjoittaminen uuteen tiedostoon tai olemassa olevan tiedoston ylikirjoittaminen:
```fish
echo "Hei, Fish Shell!" > output.txt
```
Tämä komento kirjoittaa "Hei, Fish Shell!" tiedostoon `output.txt`, luoden tiedoston, jos sitä ei ole olemassa, tai ylikirjoittaen sen, jos se on.

### Tekstin lisääminen olemassa olevaan tiedostoon:
Jos haluat lisätä tekstiä olemassa olevan tiedoston loppuun poistamatta sen nykyistä sisältöä, käytä lisäysoperaattoria `>>`:
```fish
echo "Lisätään uusi rivi tiedostoon." >> output.txt
```

### Useamman rivin kirjoittaminen:
Voit kirjoittaa useita rivejä tiedostoon käyttämällä echoa uuden rivin merkin `\n` kanssa, tai voit ketjuttaa useita echo-komentoja yhteen käyttämällä puolipisteitä:
```fish
echo "Ensimmäinen Rivi\nToinen Rivi" > output.txt
# TAI
echo "Ensimmäinen Rivi" > output.txt; echo "Toinen Rivi" >> output.txt
```

### Esimerkkituloste:
Nähdäksesi `output.txt`-tiedoston sisällön suoritettuasi yllä olevat komennot, käytä `cat`-komentoa:
```fish
cat output.txt
```
```plaintext
Ensimmäinen Rivi
Toinen Rivi
```
Tekstin korvaaminen tai lisääminen kuvatulla tavalla manipuloi tiedoston sisältöä vaatimustesi mukaisesti, osoittaen yksinkertaisia, mutta tehokkaita tapoja työskennellä tekstitiedostojen kanssa Fish Shellissä.
