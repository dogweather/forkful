---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Bash: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---

## Mitä & Miksi?

Tilapäisen tiedoston luonti tarkoittaa ohjelmistossa luodun lyhytaikaisen tiedoston, joka tekee datan väliaikaisen tallennuksen ja siirron helpoksi. Ohjelmoijat tekevät tämän, kun he tarvitsevat joitakin tietoja vain hetkellisesti tai tiedostossa olevien tietojen jakamiseksi prosessien välillä.

---

## Miten:

Esimerkki tilapäisen tiedoston luomisesta bash-ohjelmissa:

```bash
#!/bin/bash

# Luo tilapäinen tiedosto
TempTiedosto=$(mktemp)

# Tulosta tilapäisen tiedoston nimi
echo "Tilapäinen tiedosto on luotu: $TempTiedosto"
```

Aja nämä komennot pääteikkunassa:

```bash
chmod +x script.sh
./script.sh
```

Tässä on esimerkkilähtö:

```bash
Tilapäinen tiedosto on luotu: /tmp/tmp.Ijfal9W8P4
```

---

## Deep Dive:

Historiapuolella, `mktemp` -komento on johdettu Unixista ja sisällytetty GNU core utilities -kokonaisuuteen. Sen tarkoitus on helpottaa työtä tiedostojärjestelmän kanssa.

Vaihtoehtona on luoda tilapäinen tiedosto `tmpfile` -käskyä käyttämällä. Kuitenkin, `mktemp` on yleisemmin käytetty, koska se antaa sinun määrittää tiedostonimen mallin.

Sekä `mktemp` että `tmpfile` -komennot luovat tilapäisen tiedoston /tmp -kansioon. Tämä tiedosto poistetaan automaattisesti, kun suljet terminaalin tai käynnistät järjestelmän uudelleen, joten sinun ei tarvitse huolehtia sen poistamisesta.

---

## Katso Myös:

Yksityiskohtaisempi katsaus `mktemp` -komentoon: [GNU Core Utilities Documentation](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html#mktemp-invocation)

Oppikirja, joka käsittelee Bash-skriptien syvällisemmin: [The Linux Command Line, 2nd Edition](https://www.amazon.com/Linux-Command-Line-Complete-Introduction/dp/1593279523)