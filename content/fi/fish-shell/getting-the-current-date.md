---
title:                "Päivämäärän hankkiminen"
html_title:           "Fish Shell: Päivämäärän hankkiminen"
simple_title:         "Päivämäärän hankkiminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän saaminen tarkoittaa järjestelmän nykyisen päivämäärän ja ajan hakemista. Tämä on hyödyllistä, koska se auttaa ohjelmoijia esimerkiksi erilaisten aikaleimojen ja ajastettujen toimintojen luomisessa.

## Kuinka:

Fish Shellilla päivämäärän saaminen on helppoa. Käytä vain komentoa ```date```, ja nykyinen päivämäärä ja aika tulostetaan näytölle. Esimerkiksi:

```
Fish Shell > date
mauli tammi 06 10:06:23 EET 2020
```

## Syväsukellus:

Historiallisessa kontekstissa, ennen Fish Shellin käyttöä, ohjelmoijien täytyi käyttää erilaisia ohjelmointikielien kirjastoja tai suorittaa monimutkaisia komentoja saadakseen päivämäärän ja ajan. Fish Shell tekee tästä prosessista paljon helpomman yksinkertaisella ```date``` komennolla.

On myös olemassa muita vaihtoehtoja kuin Fish Shell, kuten Bash ja Z Shell, joilla on myös samanlainen ```date``` komento. Fish Shellilla tämä komento kuitenkin tukee monia erilaisia vaihtoehtoja ja parametreja, joita voit käyttää mukauttaaksesi tuloksen.

## Katso myös:

[Lisätietoja täältä.](https://fishshell.com/docs/current/cmds/date.html)