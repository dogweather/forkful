---
title:                "Fish Shell: Väliaikaistiedoston luominen"
simple_title:         "Väliaikaistiedoston luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi?

Temporary files ovat hyödyllisiä monissa ohjelmoinnin tilanteissa. Kun luomme tilapäistiedoston, voimme tallentaa väliaikaisesti tietoja jotka poistetaan kun ohjelma suorittaa loppuun. Tämä auttaa muistin hallinnassa ja estää turhia tietovuotoja.

## Kuinka luoda tilapäistiedosto Fish Shell -ohjelmassa?

```Fish Shell
set temp (mktemp)
echo "Tervetuloa, tämä on tilapäistiedosto" > $temp
cat $temp
```

Tulostus:
```Fish Shell
Tervetuloa, tämä on tilapäistiedosto
```

Voit myös määrittää tilapäistiedoston tiedostopäätteen käyttämällä komentoa "mktemp -p":

```Fish Shell
set temp (mktemp -p .temp)
cat $temp
```

Tulostus:
```Fish Shell
Tervetuloa, tämä on tilapäistiedosto
```

## Syvemmälle tilapäistiedostojen luomiseen Fish Shell -ohjelmassa

Fish Shell tarjoaa useita vaihtoehtoja tilapäistiedostojen luomiseen, kuten "mktemp", "tempfile" ja "tmpfile". Jokaisella näistä on erilaisia etuja ja haittoja, jotka sinun tulisi tutkia ennen valitsemista. Lisäksi voit hallita tilapäistiedostoja myös manuaalisesti, esimerkiksi määrittämällä tietyn tiedostonimen ja poistamalla sen käsin.

## Katso myös
- [Fish-komentotietokirja: Temporary Files](https://fishshell.com/docs/current/commands.html#temporary-files)
- [Fish-komentotietokirja: mktemp](https://fishshell.com/docs/current/commands.html#mktemp)
- [Fish-komentotietokirja: tempfile](https://fishshell.com/docs/current/commands.html#tempfile)
- [Fish-komentotietokirja: tmpfile](https://fishshell.com/docs/current/commands.html#tmpfile)