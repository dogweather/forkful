---
title:                "Uuden projektin aloittaminen"
html_title:           "Bash: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi
On monia syitä, miksi haluat aloittaa uuden Bash-projektin. Se voi olla henkilökohtainen projekti, joka auttaa sinua hallitsemaan päivittäisiä tehtäviä tai se voi olla osa suurempaa projektiyhteisöä, jossa yhteistyöskentelet muiden kanssa.

## Kuinka
### Aloitettavat asiat
Ensinnäkin sinun täytyy tietää, että Bash on Linux-järjestelmien vakio käyttöliittymä. Bash-skriptit voivat automatisoida monia tehtäviä, kuten tiedostojen kopioinnin, hakemistorakenteiden luomisen ja palvelinten hallinnan. Aloittaaksesi uuden Bash-projektin, sinun täytyy ensin varmistaa, että Bash on asennettu järjestelmääsi.

```
Bash -versio
```
```
$ GNU Bash, Versio 5.0.16 (x86_64-pc-linux-gnu)
```

### Luodaan uusi tiedosto
Seuraavaksi meidän täytyy luoda uusi tiedosto, joka sisältää Bash-skriptimme. Käytämme tähän ```touch``` komentoa.

```
Kosketa uusi_tiedosto.sh
```

### Muokkaa tiedostoa
Nyt voimme muokata uutta tiedostoa lisäämällä Bash-komentoja. Tässä esimerkissä käytämme ```echo``` komentoa tulostamaan yksinkertaisen viestin.

```
Echo "Hei maailma!"
```

### Suoritetaan skripti
Voimme suorittaa skriptimme komennolla ```bash``` ja tiedoston nimi.

```
Bash uusi_tiedosto.sh
```
Tämän tulisi tulostaa yksinkertainen viesti "Hei maailma!".

## Syvennys
Aloittaessasi uuden Bash-projektin on hyödyllistä tietää myös muita komentoja kuten ```grep```, joka auttaa tietojen hakemisessa ja ```sed```, joka auttaa tiedostojen muokkaamisessa. Voit myös luoda muuttujia Bash-skripteihin käyttämällä syntaksia ```muuttuja="arvo"``` ja käyttää niitä myöhemmin koodissasi.

## Katso myös
- [Bashin virallinen oppaita ja ohjeita](https://www.gnu.org/software/bash/manual/bash.html)
- [10 Bash-komentoa aloittelijoille](https://www.hostinger.com/tutorials/bash-commands)
- [Bash Scripting Tutorial - Tutorialspoint](https://www.tutorialspoint.com/unix/bash_scripting.htm)