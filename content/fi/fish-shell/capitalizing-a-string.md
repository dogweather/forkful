---
title:    "Fish Shell: Merkkijonon suurennus"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Tervehdys kaikille Fish Shellin käyttäjille! Oletko koskaan halunnut muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi Fish Shellillä? Tässä blogikirjoituksessa opit miksi ja kuinka tehdä se helposti.

## Kuinka tehdä se

Fish Shellillä merkkijonon ensimmäisen kirjaimen muuttaminen isoksi kirjaimeksi on yksinkertaista. Voit käyttää sisäänrakennettua `string` -komentoa, joka hyödyntää `capitalize` -funktiota. Tässä on koodiesimerkki ja siihen liittyvä tuloste:

```Fish Shell
set merkkijono "tervetuloa Fish Shellin maailmaan!"
echo $merkkijono | string capitalize
```

Tuloste:

```
Tervetuloa Fish Shellin maailmaan!
```

## Syvempi sukellus

`string` -komento pohjautuu Fish Shellin sisäänrakennettuun [pysäkki](https://fishshell.com/docs/current/cmds/eval.html) -komentoon ja `capitalize` -funktio hyödyntää Fish Shellin [kirjastoja](https://fishshell.com/docs/current/lib/index.html). Pysäkit ovat Fish Shellin tapa yhdistää useita komentoja monimutkaisten komentotulkkauksien luomiseen ja kirjastot ovat kokoelma toimintoja ja muuttujia, joita voi käyttää Fish Shellissä.

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Fish Shellin Github-sivut](https://github.com/fish-shell/fish-shell)
- [Tietoa merkkijonoista ja niiden käsittelystä Fish Shellissä](https://fishshell.com/docs/current/index.html#string-manipulation)