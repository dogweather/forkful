---
title:    "Fish Shell: Tarkastetaan, onko hakemisto olemassa"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi saattaisit haluta tarkistaa, onko kansiossa olemassa. Esimerkiksi voit käyttää tätä tarkistamaan, onko tarvittava kansio olemassa ennen tiedoston luomista, jotta voit varmistaa, että koodisi toimii oikein.

## Miten

Fish Shelliin sisäänrakennettu `test` -toiminto mahdollistaa helposti kansion olemassaolon tarkistamisen. Se palauttaa `true`, jos kansio löytyy ja `false`, jos sitä ei ole olemassa. Käytä sitä seuraavasti:

```Fish Shell
if test -d [kansion nimi]
    echo "Kansio on olemassa"
end
```

Voit myös käyttää kansion olemassaolon tarkistamiseen `set` -komennon `--query` vaihtoehtoa. Tämä palauttaa `true`, jos kansiota ei ole olemassa ja `false` muuten:

```Fish Shell
set --query [kansion nimi]
```

### Syöte ja tuloste

Syöteenä voit käyttää kansion nimeä tai polkua, joka sisältää kansion nimen. Tulosteessa `true` osoittaa, että kansio on olemassa ja `false` osoittaa, että sitä ei ole.

## Syvemmälle

Kansion olemassaolon tarkistaminen perustuu tiedostojärjestelmän olemassaolon tarkistamiseen, joka on yksi Unix-järjestelmän perusperiaatteista. Fish Shellin sisäänrakennettu `test` -toiminto käyttää taustalla olevaa `test` -komennon toteutusta, joka on myös Unix-järjestelmässä.

## Katso myös

- [Fish Shellin dokumentaatio kansioita koskevista komentoista](https://fishshell.com/docs/current/cmds/dirs.html)
- [Unix-järjestelmän perusperiaatteet](https://en.wikipedia.org/wiki/Unix_philosophy#The_Modular_Design_Principle)
- [Fish Shellin dokumentaatio `test` -toiminnosta](https://fishshell.com/docs/current/cmds/test.html)