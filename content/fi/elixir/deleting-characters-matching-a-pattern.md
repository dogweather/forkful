---
title:                "Elixir: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi Poistaa Merkkejä Jotka Vastaa Kaavaa

Elixir on funktionaalinen ohjelmointikieli, joka on suunniteltu skaalautuviksi ja joustaviksi sovellusten kehittämistä varten. Yksi Elixirin hyödyllisistä ominaisuuksista on kyky poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tässä blogikirjoituksessa tarkastellaan, miksi tämä toiminto on tärkeä ja miten sitä voi käyttää tehokkaasti.

## Miten Tehdä

Elixirissa voit käyttää `String.replace/3` -funktiota poistaaksesi merkkejä tietyn kaavan mukaan. Esimerkiksi, jos haluat poistaa kaikki välilyönnit merkkijonosta, voit tehdä seuraavasti:

```
Elixir> String.replace("Tämä on esimerkki", " ", "")
"Tämäonesimerkki"
```

Voit myös käyttää regex-kaavoja `String.replace/3` -funktion toisessa parametrissa. Esimerkiksi, jos haluat poistaa kaikki numerot merkkijonosta, voit käyttää seuraavaa koodia:

```
Elixir> String.replace("12345 on numerosarja", ~r/\d/, "")
" on numerosarja"
```

Lisäksi voit käyttää `String.replace/4` -funktiota säätääksesi kaavan käyttäytymistä. Voit esimerkiksi poistaa vain ensimmäisen esiintymän kaavasta lisäämällä parametrin `limit: 1`. Alla on esimerkki:

```
Elixir> String.replace("aabbaabb", "aa", "", limit: 1)
"bbaabb"
```

## Syvällinen Sukellus

`String.replace/3` - ja `String.replace/4` -funktioiden avulla voit poistaa merkkejä merkkijonosta vastaavien kaavojen avulla. Voit myös yhdistää näitä funktioita muiden Elixirin toimintojen kanssa, kuten `Enum.map/2`, jotta voit poistaa merkkejä useammasta merkkijonosta kerralla.

On myös tärkeää huomata, että merkkijonot ovat Elixirissa muuttumattomia, joten `String.replace/3` -funktio luo uuden merkkijonon, jossa halutut muutokset on tehty. Tämä tarkoittaa, että alkuperäinen merkkijono ei muutu.

## Katso myös

- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%25-String-Replace-3)
- [RegEx-kaavan opas](https://regexr.com/)

Kiitos että luit tämän blogikirjoituksen! Toivottavasti se auttoi sinua oppimaan kuinka poistaa merkkejä Elixirissä.