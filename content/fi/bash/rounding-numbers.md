---
title:                "Numerojen pyöristäminen"
date:                  2024-01-26T03:42:46.666327-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/rounding-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Numeroiden pyöristäminen tarkoittaa desimaalien karsimista yksinkertaisempaan arvoon, joka on riittävän hyvä tietyssä kontekstissa. Ohjelmoijat pyöristävät numeroita yksinkertaistaakseen tuloksia, säästääkseen tilaa tai koska tarkka arvo ei ole olennainen—kuten silloin, kun silmämääräisesti tarkastellaan prosessorin käyttöä tai levytilaa, ja desimaaleilla ei ole päivää kaatavaa merkitystä.

## Kuinka:

Tässä tietoisku Bashin pyöristämisestä:

```Bash
# Pyöristä alaspäin käyttäen 'floor' bc:ssä
echo "scale=0; 3.49/1" | bc

# Pyöristä ylöspäin käyttäen 'ceiling' bc:ssä
echo "scale=0; 3.01/1" | bc -l

# Pyöristä lähimpään kokonaislukuun käyttäen printf
printf "%.0f\n" 3.49

# Kikka pyöristää lähimpään kokonaislukuun käyttäen bc:tä
echo "(3.49+0.5)/1" | bc
```

Esimerkkitulosteet—suoraan terminaalista:

```
3  # Pyöristetty alas (floor)
4  # Pyöristetty ylös (ceiling)
3  # Pyöristetty lähimpään (printf-käskyllä)
3  # Pyöristetty lähimpään (bc:llä)
```

## Syväsukellus

Aikoinaan, kun Bash-skripteissä ei ollut `bc`:tä tai `printf`:iä matematiikan temppuihin, vanhan koulukunnan täytyi turvautua ulkoisiin työkaluihin tai nokkeliin kiertoteihin. Nyt `bc` mahdollistaa tarkan matematiikan tekemisen. On kuitenkin hyvä muistaa, että `bc` ei pyöristä oletusarvoisesti—se suorittaa alaspäin pyöristämisen. Scale-osio määrittää desimaalipisteen käsittelyn.

Vaihtoehtoja? Voisit käyttää `awk`:ia pyöristämiseen vaihtamatta `bc`:hen tai käsitellä `perl`:iä painavampien matematiikan tarpeiden kanssa. Masokisteille, mene puhdas Bash esimerkiksi iteratiivisella merkkijonomanipulaatiolla – mutta miksi?

Yksityiskohtien osalta, `bc` ei vain pyöristä, se tekee paljon matematiikkajuttuja—skaalaa, sinittää, neliöjuurittaa, sinä nimität sen. `printf` on enemmän tekstien muotoilusta, mutta hei, se pyöristää numeroita, joten emme valita.

## Katso Myös

Niille, jotka haluavat lisää:

- GNU `bc` manuaali: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bashin `printf` komento: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK käyttäjän opas (pyöristämiseen ja muuhun tekstin käsittelyyn): https://www.gnu.org/software/gawk/manual/gawk.html
- Lisää Bash matematiikkaa, skriptaus- ja numerotemppuja: https://mywiki.wooledge.org/BashFAQ/022