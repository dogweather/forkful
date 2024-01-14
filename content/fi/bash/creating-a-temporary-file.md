---
title:    "Bash: Väliaikaisen tiedoston luominen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda tilapäistiedosto?

Tilapäisten tiedostojen luominen on tärkeä osa Bash-ohjelmointia, sillä se antaa meille mahdollisuuden tallentaa ja käsitellä väliaikaista tietoa. Tämä on erityisen hyödyllistä silloin, kun haluamme luoda väliaikaisia tietokantoja tai tallentaa käyttäjän syötteitä. Kaiken kaikkiaan tilapäisten tiedostojen luominen tekee Bash-ohjelmoinnista tehokkaampaa ja helppokäyttöisempää.

## Miten luoda tilapäistiedosto?

Tilapäisen tiedoston luominen Bashilla on yksinkertaista. Voimme käyttää `mktemp`-komentoa luodaksemme uniikin tilapäistiedoston, joka poistetaan automaattisesti, kun Bash-ohjelma päättyy. Esimerkiksi, jos haluamme luoda tilapäistiedoston nimeltä "temp.txt", meidän tarvitsee vain kirjoittaa seuraava koodi:

```Bash
tempfile=$(mktemp temp.txt)
```

Voimme myös käyttää `mktemp`-komennon parametreja määrittelemään tilapäistiedoston sijainnin tai nimen. Tämä antaa meille enemmän joustavuutta tiedostonhallinnassa.

## Syvempi sukellus

Tilapäisten tiedostojen luominen tapahtuu käyttämällä Unix-järjestelmien `tempnam()`-funktiota. Tämä funktio luo uniikin tiedostonimen ja palauttaa sen käyttäjälle. Bash käyttää tätä funktiota `mktemp`-komentonsa kautta.

Meidän pitää myös muistaa, että vaikka tilapäinen tiedosto poistetaan automaattisesti Bash-ohjelman päätyttyä, se ei takaa tiedoston välitöntä poistamista. Tämä tarkoittaa, että meidän pitää huolehtia tarvittaessa manuaalisesti tilapäisten tiedostojen poistamisesta.

# Katso myös

- [Linux Foundationin Bash Scripting Guide](https://linuxcommand.org/lc3_wss0010.php)
- [Bash-ohjelmoinnin perusteet](https://www.katacoda.com/courses/bash/introduction)
- [Tilapäisten tiedostojen lopullinen poisto](https://linuxcommando.blogspot.com/2008/06/clean-up-temporary-files-in-linux.html)