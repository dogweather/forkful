---
title:                "Fish Shell: Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monilla ohjelmointikielet tarjoavat sisäänrakennettuja toimintoja, jotka helpottavat datan käsittelyä. Fish Shellissa yksi näistä toiminnoista on merkkijonomuuttujien käsittely. Kapitalisointitoiminnon avulla käyttäjä voi muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja loput kirjaimet pieniksi. Tässä blogikirjoituksessa opit, miten voit hyödyntää tätä toimintoa Fish Shell -ohjelmoinnissa.

## Näin teet sen

Fish Shellissa merkkijonon kapitalisointi tapahtuu käyttämällä sisäänrakennettua `capitalize` -toimintoa. Se ottaa parametrinaan merkkijonon ja palauttaa uuden merkkijonon, jossa ensimmäinen kirjain on isolla ja loput pienellä.

```
Fish Shell 
capitalize "esimerkki"```

Tämä palauttaa "Esimerkki".


```
Fish Shell
capitalize "FINNISH"``
Tämä palauttaa "Finnish".

Voit myös ketjuttaa useita toimintoja yhteen saadaksesi haluamasi tuloksen, esimerkiksi:

```
Fish Shell
capitalize "fish" | append ", the friendly shell” | append " is" | append " awesome!"```

Tämä palauttaa "Fish, the friendly shell is awesome!"

## Syvällisemmin

Kapitalisointitoiminto tarjoaa helpon tavan muokata merkkijonoja Fish Shell -ohjelmoinnissa. Tämän toiminnon lisäksi Fish Shellissa on myös muita tapoja muokata merkkijonoja, kuten `join`, `replace`, `substr` ja `split`. Näiden toimintojen yhdisteleminen mahdollistaa monimutkaisempien merkkijonokäsittelyjen suorittamisen.

Fish Shell tarjoaa myös mahdollisuuden muokata ympäristömuuttujia, jotka ovat suosittu tapa säilyttää tietoa ohjelmien välillä. `set` -toiminnon avulla voit asettaa uuden muuttujan arvon ja `get` -toiminto antaa sinulle kyseisen muuttujan arvon.

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Opas merkkijonokäsittelyyn Fish Shellissä](https://blog.crashmaster.me/post/fish-string-manipulation/)
- [Fish Shellin komentorivivinkkejä](https://www.linode.com/docs/guides/fish-shell-tips-and-tricks/)