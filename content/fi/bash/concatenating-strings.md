---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Bash: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi
Joskus Bash-skripteissä on tarvetta yhdistellä eri tekstinpätkiä yhdeksi uudeksi merkkijonoksi. Tämän artikkelin avulla opit helposti, miten tämä onnistuu.

## Miten
Bashissa on mahdollista yhdistellä merkkijonoja käyttämällä `+`-merkkiä. Alla olevassa esimerkissä yhdistämme kaksi merkkijonoa ja tulostamme ne:

```Bash
teksti1="Tämä on"
teksti2="esimerkki"
echo $teksti1 + $teksti2
```

Tulostus:
```
Tämä on + esimerkki
```

Huomaa, että `+`-merkki lisää merkkijonojen väliin välilyönnin. Voit myös yhdistää useampia merkkijonoja yhteen, esimerkiksi:

```Bash
teksti1="Tämä"
teksti2="on"
teksti3="esimerkki"
echo $teksti1$teksti2$teksti3
```

Tulostus:
```
Tämäonesimerkki
```

## Syvemmälle
Voit myös käyttää erikoismerkkejä, kuten newline `\n` ja välilyönti `\t` yhdistämään merkkijonoja:

```Bash
teksti1="Tämä on"
teksti2="esimerkki"
yhdista="$teksti1\n\t$teksti2"
echo -e $yhdista
```

Tulostus:
```
Tämä on
    esimerkki
```

Voit myös käyttää Bashin sisäänrakennettua `printf`-toimintoa yhdistämään merkkijonoja:

```Bash
teksti1="Tämä"
teksti2="on"
teksti3="esimerkki"
printf "%s %s %s" $teksti1 $teksti2 $teksti3
```

Tulostus:
```
Tämä on esimerkki
```

## Katso myös
- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash-komennot Cheat Sheet](https://devhints.io/bash)
- [Learn Bash in Y Minutes](https://learnxinyminutes.com/docs/bash/)