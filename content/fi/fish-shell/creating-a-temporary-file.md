---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tilapäiset tiedostot ovat väliaikaisia tiedostoja, joita ohjelmoijat luovat tiedon varastoimiseksi lyhytaikaiseen käyttöön. Niitä käytetään usein, kun meillä on suuria tietomääriä, joita ei voi tai ei haluta pitää muistissa kerralla.

## Miten:

Katso allaolevaa Fish Shellin esimerkkikoodia muistilapputiedoston syntymisen näkemiseksi.

```Fish Shell
# Luodaan väliaikainen tiedosto
set tmpfile (mktemp)

# Käytetään väliaikaista tiedostoa
echo "Hei maailma" > $tmpfile
cat $tmpfile
```

Koodin tuloste olisi jotain seuraavanlaista:

```Fish Shell
Hei maailma
```

## Syvempi sukellus:

Väliaikaisten tiedostojen syntymisen idea on ollut käytössä pitkään, ja se on nähnyt monia toteutustapoja eri ympäristöissä. Fish Shell käyttää `mktemp`-komentoa, joka on peräisin Unix-järjestelmistä. Se on suora ja yksinkertainen tapa luoda väliaikainen tiedosto.

On olemassa useita vaihtoehtoja, mukaan lukien `/dev/shm` tai muita ns. RAM-levyjä. Kaikilla on etunsa ja haittansa, riippuen tietysti käyttötarkoituksesta.

Tiedostojen luonti Fish Shellissä on varsin suoraviivaista. `mktemp`-komento luo uuden väliaikaisen tiedoston ja palauttaa sen polun, jonka voimme sitten tallentaa muuttujaan.

## Katso myös:

- Fish Shell -käyttöopas: https://fishshell.com/docs/current/index.html
- Väliaikaisten tiedostojen käyttö Unix-järjestelmissä: https://unix.stackexchange.com/questions/84991/benefits-of-mktemp-over-dev-shm