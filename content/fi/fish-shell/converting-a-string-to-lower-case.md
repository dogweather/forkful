---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Muuttujan muuttaminen pieniksi kirjaimiksi (eng. converting a string to lower case) tarkoittaa ohjelmointia, jossa muutetaan merkkijonon kaikki kirjaimet pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän usein normalisoidakseen tietoja, kuten käyttäjätunnuksia tai sähköpostiosoitteita.

## Näin se tehdään:
Fish Shellissä voit yksinkertaisesti käyttää `string` komentoa muuttaaksesi merkkijonon pieniksi kirjaimiksi. 

```Fish Shell
set muuttuja "Moi Maailma"
echo $muuttuja | string lower
```

Tämä tulostaa: `moi maailma`

## Syvempi sukellus
Fish Shell, vuonna 2005 julkaistu komentotulkki, lisäsi 'string' toiminnon version 2.3.0 myötä, mikä helpottaa merkkijonoihin liittyviä operaatioita. Vaihtoehtona voitaisiin käyttää `tr` komentoa `tr '[:upper:]' '[:lower:]'`, mutta se saattaa tuottaa odottamattomia tuloksia joillakin Unicode merkkijonoilla. Fishin 'string lower' käyttää kansainvälistä komponenttia, mikä takaa laajemman merkkijonon tuen.

## Katso myös
- Fishin virallinen dokumentaatio: [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- Yksityiskohtaisempi artikkeli merkkijonojen muuntamisesta: [Converting Strings in Fish](https://riptutorial.com/fish-shell/example/20341/converting-strings)