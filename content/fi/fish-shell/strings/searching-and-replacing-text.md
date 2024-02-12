---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- /fi/fish-shell/searching-and-replacing-text/
date:                  2024-01-20T17:57:56.354089-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Tekstin etsiminen ja korvaaminen auttaa muuttamaan koodia nopeasti. Se on tärkeää, kun halutaan päivittää muuttujien nimiä tai korjata virheitä useissa tiedostoissa kerralla.

## How to: - Kuinka Tehdä:
Fish Shellissä voit etsiä ja korvata tekstiä `string` komennon avulla. Tässä on esimerkit:

```Fish Shell
# Yksinkertainen korvaus
echo "kala on hyvää" | string replace "kala" "lohi"

# Tulostuu: lohi on hyvää

# Globaali korvaus tiedostossa
string replace -a "vanha" "uusi" < tiedosto.txt > korjattu_tiedosto.txt

# Kahdella tiedostolla samaan aikaan
string replace -a "bugi" "ominaisuus" < vanha_koodi.fish > uusi_koodi.fish
string replace -a "bugi" "ominaisuus" < vanha_dokumentti.md > uusi_dokumentti.md
```

## Deep Dive - Syväsukellus
Sananen historiasta: UNIX-järjestelmissä tekstinkäsittely on ollut perustoimintoja alusta alkaen. `sed` ja `awk` olivat alkuun. Fish Shellin `string` komento on uudempi lisäys, joka tekee saman homman, mutta yksinkertaisemmin ja modernimmin.

Vaihtoehtoja: Voit käyttää `sed`, `awk`, tai jopa `perl` ja `python` skriptejä tekstinkäsittelyyn. Fishin `string` on kuitenkin integratoitu suoraan shelliin, mikä tekee siitä nopean ja vaivattoman valinnan.

Tarkemmin toteutuksesta: Fishin `string` komennossa on monia optioita, kuten `-i` ignoroimaan kirjainkoko, tai `-r` käyttämään säännöllisiä lausekkeita hakuehtoina. Se käyttää Fishin sisäistä string-käsittelyä, mikä on tehokasta ja muistaa Unicode tuen.

## See Also - Katso Myös
- Fish Shellin dokumentaatio `string` komennosta: https://fishshell.com/docs/current/cmds/string.html
- UNIX `sed` komennon yleiskatsaus: https://www.gnu.org/software/sed/manual/sed.html
- `awk`-ohjelmointikielen esittely: https://www.gnu.org/software/gawk/manual/gawk.html
- Säännölliset lausekkeet, syvällinen opas: https://www.regular-expressions.info/
