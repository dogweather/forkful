---
title:                "Fish Shell: Trookka kirjoittamisesta standarivirheelle."
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaisit standardivirheelle?

Kirjoittaminen standardivirheelle on tärkeä osa Fish Shell ohjelmointia. Se mahdollistaa virheiden hallinnan ja vianmäärityksen helpottamisen ohjelmointiprosessissa.

## Kuinka tehdä se?

Fish Shell tarjoaa kätevän ja helpon tavan kirjoittaa standardivirheelle käyttämällä "echo" komentoa ja ">2" -osaa sen perään, kuten alla olevassa esimerkissä:

```
echo "Tämä on virheilmoitus" >2
```

Tämä komento kirjoittaa ilmoituksen standardivirheelle ja antaa käyttäjälle selkeän viestin siitä, mitä on tapahtunut.

## Syvempää tietoa

Kirjoittaminen standardivirheelle mahdollistaa myös virheiden raportoinnin ja käsittelyn skripteissä ja ohjelmissa. Käyttämällä "stderr" muuttujaa, voit ohjata standardivirhettä erityisesti haluamaasi paikkaan. Esimerkiksi:

```
echo "Tämä on virheilmoitus" 2> $stderr
```

Tämä ohjaa virheilmoituksen muuttujaan "stderr", josta voit sitten käsitellä sitä haluamallasi tavalla.

# Katso myös

- Fish Shell viralliset ohjeet: https://fishshell.com/docs/current/index.html
- Standardivirheen käyttö Shellissä: https://techstop.github.io/2016/11/24/errors-stdout-stderr/
- Shell Scripting Tutorial: https://www.shellscript.sh/