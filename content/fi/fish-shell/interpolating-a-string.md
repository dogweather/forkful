---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Interpoloiminen on koodauskikka, jossa muuttujan arvo sijoitetaan suoraan merkkijonoon. Se tekee ohjelmoinnista huomattavasti sujuvampaa, koska erillisiä kutsuja muuttujan arvon muuntamiseksi merkkijonoksi ei tarvita.

## Miten Näin:

```Fish Shell
# Muuttujan luominen
set kala "Fish Shell"

# Merkkijonon Interpolointi
echo "Opetellaan $kala"

# Tulostaa: Opetellaan Fish Shell
```
Helppoa kuin heinänteko, eikö vain?

## Syvempi Sukellus:

(1) Historia: Interpoloinnin konsepti on läsnä useissa ohjelmointikielissä, kuten Pythonissa, Rubyssa ja nyt myös Fish Shellissä.

(2) Vaihtoehdot: Fish Shellissä $-symbolin avulla voidaan suorittaa interpolointi, mutta muita ohjelmointikieliä varten on erilaisia metodeja.

(3) Käyttö: Käytännössä, Fish Shell sijoittaa $-symbolin sisään kirjoitetun muuttujan arvon paikalleen ja käsittelee koko lauseen tai koodirivin yhtenä yksikkönä.

## Katso Myös:

Fish Shell dokumentaatio: [Fish Scripting Manual](https://fishshell.com/docs/current/index.html)

Opi lisää merkkijonojen interpoloinnista: [Merkkijonojen interpolointi](https://www.baeldung.com/cs/string-interpolation)