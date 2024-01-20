---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Nykyisen päivän hankkiminen Fish Shellillä

## Mitä ja Miksi?

Nykyisen päivämäärän hankkiminen tarkoittaa päivämäärän ja ajan hankkimista juuri sillä sekunnilla, kun koodi suoritetaan. Tätä tehdään lähinnä ajanseurantaa ja tapahtumien lokitusta varten.

## Kuinka:

Tässä on esimerkkikoodi ja sen tulostus Fish Shellillä:

```Fish Shell
set -l CURRENT_DATE (date)
echo $CURRENT_DATE
```
Tämä koodi tulostaa nykyisen päivämäärän ja ajan, esimerkiksi: 

```Shell Output
Ti Huhti 20 15:30:27 EEST 2022
```

## Syvällisempi tarkastelu:

(1) Historiallinen konteksti: Fish Shell syntyi 2005 pyrkimyksenä luoda käyttäjäystävällisempi komentotulkki.

(2) Vaihtoehtoja nykyisen päivämäärän hankkimiseen ovat bash ja zsh, mutta kumpikin edellyttää pidempää komentoa samaan tehtävään.

(3) Tarkemmat tiedot: Fish Shell hankkii nykyisen päivämäärän käyttäen `date`-komentoa, joka on osa Unix-järjestelmän ydintoiminnallisuutta.

## Katso myös:

Fish Shellin virallinen verkkosivu: [Fish Shell](https://fishshell.com/)

Unix-komentojen opas: [Unix Commands](https://unixguide.net/unix/bash_t.shtml)

Fish Shellin käytön opas: [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)