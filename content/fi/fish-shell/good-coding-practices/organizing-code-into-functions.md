---
aliases:
- /fi/fish-shell/organizing-code-into-functions/
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:02:32.082544-07:00
description: "Koodin j\xE4rjest\xE4minen funktioihin on osioiden kasaamista skriptist\xE4\
  \ tiettyj\xE4 teht\xE4vi\xE4 varten. Teemme sen, koska se tekee koodista helpommin\
  \ luettavaa,\u2026"
lastmod: 2024-02-18 23:09:08.090304
model: gpt-4-0125-preview
summary: "Koodin j\xE4rjest\xE4minen funktioihin on osioiden kasaamista skriptist\xE4\
  \ tiettyj\xE4 teht\xE4vi\xE4 varten. Teemme sen, koska se tekee koodista helpommin\
  \ luettavaa,\u2026"
title: "Koodin j\xE4rjest\xE4minen funktioiksi"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioihin on osioiden kasaamista skriptistä tiettyjä tehtäviä varten. Teemme sen, koska se tekee koodista helpommin luettavaa, testattavaa ja uudelleenkäytettävää – kukaan ei halua kahlata läpi koodispagetin suota.

## Kuinka:
Fishissä kirjoitat funktion `function` avainsanalla, annat sille nimen ja lopetat `end`-sanan kanssa. Tässä on yksinkertainen esimerkki:

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

Nyt tehdään siitä käyttäjää tervehtivä:

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

Tallentaaksesi sen käyttöistuntojen välille, käytä `funcsave greet`.

## Syväsukellus
Fish Shellin funktiot ovat kuin pieniä skriptejä – voit tunkea sinne melkein mitä tahansa. Historiallisesti funktioiden konsepti kuoriskriptauksessa on säästänyt lukemattomia tunteja toistuvaa kirjoittamista ja debuggausta. Toisin kuin ohjelmointikielissä kuten Python, Shell-funktiot ovat enemmän mukavuudesta kuin rakenteesta.

Joissakin kuorissa, kuten Bashissa, käytetään `function` avainsanaa tai suoraan aaltosulkeita. Fish pysyy `function ... end` linjassa – selkeä ja luettava. Fish-funktioissa saat kaikki härpäkkeet: parametrit, paikalliset muuttujat `set -l` komennolla, ja voit jopa määritellä funktion toisen funktion sisällä.

Et tarvitse `return` arvoa, koska Fish ei ole siitä kiinnostunut; funktion tuloste on sen palautus. Ja jos haluat pysyviä funktioita käytettäväksi tulevissa istunnoissa, muista `funcsave`.

## Katso Myös

- Fish-tutoriaali funktioista: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Funktion komentosarjat

- [function](https://fishshell.com/docs/current/cmds/function.html) — Luo funktio
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Tulosta tai poista funktioita
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Tallenna funktion määritelmä käyttäjän automaattiseen latauskansioon
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Muokkaa funktiota interaktiivisesti
