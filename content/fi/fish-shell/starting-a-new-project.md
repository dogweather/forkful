---
title:                "Fish Shell: Uuden projektin aloittaminen"
programming_language: "Fish Shell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Aloittaessa uuden projektin, on tärkeää valita oikea ohjelmointikieli. Fish Shell on moderni ja helppokäyttöinen vaihtoehto, joka sopii hyvin erilaisiin projekteihin.

## Miten

Fish Shellin käyttöönotto on helppoa ja nopeaa. Seuraa seuraavia vaiheita:

1. Asenna Fish Shell käyttöjärjestelmällesi komennolla `sudo apt-get install fish` (Ubuntu) tai `brew install fish` (OS X)
2. Luo uusi projekti hakemistoon `mkdir project`
3. Siirry uuteen hakemistoon komennolla `cd project`
4. Luo uusi tiedosto `touch script.fish`
5. Avaa tiedosto tekstieditorilla ja kirjoita seuraava koodi:

```Fish Shell
#!/usr/bin/env fish

echo "Hello world!"
```

6. Tallenna ja sulje tiedosto
7. Käynnistä tiedosto komennolla `./script.fish`
8. Näet tulosteena "Hello world!"

## Syventävä tieto

Fish Shell tarjoaa monipuolisia ominaisuuksia erilaisten projekteiden hallintaan. Voit esimerkiksi luoda funktioita, joita voit käyttää usein toistuvien tehtävien suorittamiseen. Voit myös muokata Fish Shellin asetuksia ja mukauttaa sitä omiin tarpeisiisi. Lisätietoja Fish Shellin ominaisuuksista ja käytöstä löydät viralliselta verkkosivustolta: https://fishshell.com/

## Katso myös

- [Fish Shellin virallinen verkkosivusto](https://fishshell.com/)
- [Fish Shellin GitHub-sivu](https://github.com/fish-shell/fish-shell)
- [Fish Shellin dokumentaatio](https://fishshell.com/docs/3.1/index.html)