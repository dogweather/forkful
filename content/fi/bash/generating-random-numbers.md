---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaisten numeroiden generoiminen tarkoittaa ennalta arvaamattomien numeroiden luomista. Ohjelmoijat tarvitsevat näitä toimintoja esimerkiksi simulointeja, testejä, ja satunnaisuutta vaativia toimintoja varten.

## Kuinka:

Generoidaan ensimmäinen satunnainen numero Bashissa:

```Bash
echo $RANDOM
```

Esimerkiksi, tämä voi palauttaa:

```Bash
26822
```

Voit myös rajoittaa numeron tiettyyn välille, esimerkiksi välille 1-100:

```Bash
echo $((RANDOM % 100))
```

Ja tuotos saattaa olla:

```Bash
57
```

## Syvennytään:

Historiallisessa kontekstissa `$RANDOM` periytyy alkuperäisestä Bourne Shellistä, ja se on ollut Bashissa käytössä alusta saakka. 

Vaihtoehtoisesti, voit käyttää `/dev/urandom` tai `/dev/random` laitteita entistä ennalta-arvaamattomampien numeroiden generointiin.

`$RANDOM` generoi 15-bitin satunnaisen numeron, joten sen enimmäisarvo on 32767. Tätä voi muuttaa käyttämällä modulo-operaattoria `%`.

## Katso myös:

1. Bash manuaali: [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
2. Stack Overflow: [How to generate a random number in Bash?](https://stackoverflow.com/questions/8988902/how-to-generate-a-random-number-in-bash)