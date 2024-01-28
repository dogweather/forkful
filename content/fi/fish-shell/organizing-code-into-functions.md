---
title:                "Koodin järjestäminen funktioihin"
date:                  2024-01-26T01:10:26.184572-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioihin tarkoittaa koodinpätkien niputtamista tiettyjen tehtävien suorittamiseen. Teemme niin, koska se tekee koodista helpommin luettavaa, testattavaa ja uudelleenkäytettävää — kukaan ei halua rämpiä läpi spagettikoodin suon.

## Kuinka:
Fishissä kirjoitat funktion käyttämällä `function`-avainsanaa, annat sille nimen ja lopetat `end`-sanan kanssa. Tässä on yksinkertainen esimerkki:

```fish
function hello
    echo "Hello, World!"
end

hello
```

Tuloste:
```
Hello, World!
```

Tehdään seuraavaksi toivottava tervehdys käyttäjälle:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Tuloste:
```
Hey there, käyttäjänimesi!
```

Tallentaaksesi sen yli istuntojen käytä `funcsave greet`.

## Syväsukellus
Fish Shellin funktiot ovat kuin miniskriptit — voit työntää sinne melkein mitä vain. Historiallisesti funktioiden konsepti skriptauksessa on säästänyt lukemattomia tunteja toistuvaa näppäimistön hakkaamista ja virheenkorjausta. Toisin kuin ohjelmointikielissä, kuten Python, Shellin funktiot ovat enemmän mukavuudesta kuin rakenteesta.

Jotkin shellit, kuten Bash, käyttävät `function`-sanaa tai ihan vain aaltosulkeita. Fish pysyttelee `function ... end` -rakenteessa — selkeä ja luettava. Fish-funktioilla saat käyttöösi kaikki herkut: parametrit, paikalliset muuttujat `set -l`-komennon avulla, ja voit jopa määritellä funktion toisen funktion sisällä.

Et tarvitse `return` arvoa, koska Fish ei pane painoa sille; funktion tuloste on sen palautusarvo. Ja jos haluat pysyviä funktioita käytössä tulevia istuntoja varten, muista käyttää `funcsave`.

## Katso myös
- Fishin opas funktioista: https://fishshell.com/docs/current/tutorial.html#tut_functions
- Fishin dokumentaatio `function`-komennosta: https://fishshell.com/docs/current/cmds/function.html
- Kattava opas funktioiden kirjoittamiseen fishissä: https://fishshell.com/docs/current/index.html#syntax-function
