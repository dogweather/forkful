---
title:    "Fish Shell: Tekstitiedoston lukeminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko oppia lukemaan tekstitiedostoja Fish Shell -ohjelmoinnissa? On monia syitä, miksi haluat tehdä niin: ehkä haluat käsitellä suuria määriä tietoa, tehdä muutoksia olemassa oleviin tiedostoihin tai vain yksinkertaisesti näyttää tiedot käyttäjälle.

## Miten

Aloita avaamalla terminaali ja käynnistämällä Fish Shell. Seuraavaksi, navigoi kansioon, jossa haluat käsitellä tekstitiedostoa. Käytä sitten komentoa `cat`, joka näyttää tiedoston sisällön terminaaliin. Esimerkiksi:

```Fish Shell

cat tiedosto.txt
```

Tämä näyttää tiedoston sisällön ruudullasi. Voit myös käyttää `less` komentoa, joka näyttää tiedoston sisällön sivuittain ja antaa sinun selata tekstin läpi käyttämällä nuolinäppäimiä. Esimerkiksi:

```Fish Shell

less tiedosto.txt
```

## Syväsukellus

Fish Shell -ohjelmoinnissa on monia tapoja lukea ja käsitellä tekstitiedostoja. Voit esimerkiksi käyttää erilaisia komentoja sisäänrakennetun `read` toiminnon avulla, joka lukee tiedoston sisällön muuttujaan. Voit myös käyttää `grep` komentoa etsimään tiettyä tekstiä tiedostosta tai `awk` komentoa muokkaamaan ja järjestämään tiedot haluamallasi tavalla.

## Katso myös

- [Fish Shell -ohjelmointiopas](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell -komennon viiteopas](https://fishshell.com/docs/current/commands.html)
- [Fish Shell -skriptausopas](https://fishshell.com/docs/current/index.html)