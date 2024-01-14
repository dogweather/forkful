---
title:    "Fish Shell: Tarkistetaan löytyykö hakemistoa"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit tarkistaa, onko hakemistoa olemassa? Ehkä tarvitset ohjelmassasi tiettyä polkua tai haluat varmistaa, että tietty hakemisto on olemassa ennen kuin jatkat sen käyttöä.

## Miten

Fish Shellissa on helppo tarkistaa, onko hakemisto olemassa käyttämällä `test` komentoa ja `-d` flagia. Tämä tarkistaa, onko olemassaolon lisäksi kyseessä myös hakemisto.

```
Fish Shell kayttaen

test -d hakemisto
```

Tulosteena saat joko `true` tai `false` riippuen siitä, onko hakemisto olemassa.

```
true
```

## Syventävä sukellus

`test` komento tukee myös muita flagia, joita voit käyttää hakemistojen tarkistamiseen. Esimerkiksi `-f` flagin avulla voit tarkistaa, onko kyseessä tiedosto tai `-L` flagilla voit tarkistaa symbolisen linkin olemassaolon.

On myös mahdollista käyttää `if` ehtolauseita tarkistamiseen, jolloin voit määrittää tiettyjä toimenpiteitä, jotka suoritetaan vain jos hakemisto ei ole olemassa.

## Katso myös

- [Fish Shell test documentation](https://fishshell.com/docs/current/cmds/test.html)
- [DigitalOcean article on working with files in Fish Shell](https://www.digitalocean.com/community/tutorials/how-to-work-with-files-using-the-fish-shell)