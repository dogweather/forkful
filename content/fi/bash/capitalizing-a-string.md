---
title:                "Merkkijonon isoilla kirjaimilla"
html_title:           "Bash: Merkkijonon isoilla kirjaimilla"
simple_title:         "Merkkijonon isoilla kirjaimilla"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Bash-ohjelmointi: Merkkijonojen Suuraakkosten Käyttö 
## Mikä & Miksi?
Suuraakkosten käyttö muuntaa merkkijonon kaikki kirjaimet suuriksi. Tämä on hyödyllistä esimerkiksi kun halutaan välttyä kirainkoista johtuvilta virheiltä tai kun halutaan tehdä vertailuja, jotka ovat riippumattomia kirjainkoosta.

## Kuinka:
Merkkijonon muuntaminen suuraakkosiksi Bashissa on helppoa. Tässä on esimerkki.

```Bash
teksti="hello world"
teksti=${teksti^^}
echo $teksti
```
Näytön tulos olisi:

```Bash
HELLO WORLD
```

## Tarkempi tarkastelu:
Suuraakkosten muunnos kehittyi samassa yhteydessä ASCII-merkkijärjestelmän kanssa, joka erottaa pien- ja suuraakkoset. Vaikka Bash tarjoaa kätevän suuraakkosten muunnoksen, muita tapoja on olemassa. Awkin tai sed-komentojen avulla voidaan käyttää säännöllisiä lausekkeita mukautettuun muunnokseen. Ja sisäiset yksityiskohdat? Tämä muutos käyttää sisäisesti C:n toupper-funktiota.

## Katso myös:
1. Bash Programming Guide: https://tiswww.case.edu/php/chet/bash/bashref.html  
2. Awk-ohjelma: https://www.gnu.org/software/gawk/manual/gawk.html  
3. Sed-komento: https://www.gnu.org/software/sed/manual/sed.html