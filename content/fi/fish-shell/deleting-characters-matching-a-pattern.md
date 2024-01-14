---
title:                "Fish Shell: Mallia vastaavien merkkien poistaminen"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Miksi Poistaa Merkkejä, Joita Vastaavat Kaavat?

Merkkejä vastaavien kaavojen poistaminen on hyödyllistä, kun haluat puhdistaa tietoja tai suodattaa tiettyjä merkkijonoja. Tämä voi myös auttaa muokkaamaan tai parantamaan koodia.

# Kuinka Tehdä Se?

```Fish Shell ``` tarjoaa käteviä keinoja poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Alla on joitakin esimerkkejä: 

```
rm *\.txt
```

Tämä poistaa kaikki .txt-tiedostot hakemistosta. Voit myös käyttää säännöllisiä lausekkeita poistaaksesi tiettyjä merkkejä, kuten seuraavassa esimerkissä:

```
rm (echo abc | sed 's/b/#/')
```
Tämä poistaisi merkin "b" ja korvaisi sen merkillä "#" merkkijonossa "abc".

# Syvemmälle Aiheeseen

Säännölliset lausekkeet ovat erittäin hyödyllisiä poistettaessa merkkejä, jotka vastaavat tiettyä kaavaa. Voit käyttää myös muita komentoja, kuten ```grep``` ja ```sed```, löytääksesi ja muokataksesi haluamiasi merkkijonoja. 

# Katso Myös

- https://fishshell.com/docs/current/commands.html#rm
- https://fishshell.com/docs/current/commands.html#sed
- https://fishshell.com/docs/current/commands.html#grep
- https://fishshell.com/docs/current/commands.html#regex