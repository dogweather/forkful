---
title:                "Tietokoneohjelmointi: Kirjoittaminen standardivirheelle"
html_title:           "Fish Shell: Tietokoneohjelmointi: Kirjoittaminen standardivirheelle"
simple_title:         "Tietokoneohjelmointi: Kirjoittaminen standardivirheelle"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheelle voi olla hyödyllistä, jos haluat nähdä lopputuloksen välittömästi ja varmistaa, että kaikki toiminnot suoritetaan oikein.

## Kuinka tehdä se

Kun haluat kirjoittaa standardivirheelle Fish Shell -ohjelmassa, käytä seuraavaa koodia:

```
echo "Tämä on standardivirheen teksti" >&2
```
Tämä koodi kirjoittaa "Tämä on standardivirheen teksti" -tekstin standardivirheelle, joten se näkyy komentorivillä.

## Syvemmälle

Standardivirheen kirjoittaminen voi olla hyödyllistä myös silloin, kun haluat ohjelman tuottavan virheviestin tietyissä tilanteissa. Voit esimerkiksi käyttää seuraavaa koodia tarkistaaksesi, onko käyttäjä syöttänyt ohjelmaan oikean määrän argumentteja:

```
if test (count $argv) -ne 2
    echo "Ohjelmaan tulee syöttää kaksi argumenttia" >&2
    exit 1
end
```
Tässä tapauksessa ohjelma kirjoittaa standardivirheelle tekstin "Ohjelmaan tulee syöttää kaksi argumenttia" ja lopettaa suorituksen virhekoodilla 1, jos käyttäjä ei anna kahta argumenttia.

## Katso myös

- [Fish Shellin viralliset dokumentaatiot standardiovirheen käyttämisestä](https://fishshell.com/docs/current/commands.html#step-9-redirecting-stderr)
- [Ohjelmoinnin perusteet: Tekstin kirjoittaminen standardivirheelle](https://ohjelmointiopas.fi/fish-shell/standardivirheen-kaytto/)