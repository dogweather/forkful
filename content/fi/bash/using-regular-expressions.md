---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Säännölliset lausekkeet ovat syötteiden käsittelyyn tarkoitettu työkalu, ne ovat tehokkaita mallinsovitustekniikoita. Ohjelmoijat käyttävät niitä tiedostojen ja datan läpikäynnin tehostamiseen haarautumis- ja silmukkatietojen sijaan.

## Kuinka:
Tässä on joitain esimerkkejä:
```Bash
# Etsii tekstiä
grep "etsi minua" tiedosto.txt

# Regex or-kysely
grep "sana1\sana2" tiedosto.txt

# Regex ja-kysely
grep -P "sana1.*sana2|sana2.*sana1" tiedosto.txt
``` 
Tulostettava tulos näyttää suurin piirtein tältä:
```Bash
# Otteita tiedosto.txt:stä
etsi minua
sana1 sana2
```
## Syvällisempi katsaus
Shell-scriptauksen historia on pitkä, se ulottuu aina 1970-luvulle. Säännöllisten lausekkeiden vaihtoehtoja ovat esimerkiksi AWK ja Sed, mutta grep-yhdistelmän yksinkertaisuus ja nopeus tekevät siitä erittäin suositun. Nykyisin, säännöllisten lausekkeiden toteutus voi vaihdella hieman eri järjestelmissä, mutta yleiset periaatteet pysyvät ennallaan.

## Katso myös
1. [GNU Grep Manuaali](https://www.gnu.org/software/grep/manual/grep.html) - Lisätietoja ja usein kysyttyjä kysymyksiä.
2. [RegExr](https://regexr.com/) - Opettele, rakenna ja testaa RegEx.
3. [Sed - An Introduction and Tutorial](https://www.grymoire.com/Unix/Sed.html) - Lisää vaihtoehtoja säännöllisille lausekkeille.
4. [AWK - A Tutorial and Introduction](https://www.grymoire.com/Unix/Awk.html) - Lisää korkeamman tason skriptikieliä tiedostojen käsittelyyn.