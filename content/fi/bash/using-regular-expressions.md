---
title:    "Bash: Säännöllisten lausekkeiden käyttö"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Regular expressions (säännölliset lausekkeet) ovat voimakas työkalu ohjelmoinnissa, joka auttaa etsimään ja manipuloimaan tekstiä. Niitä käytetään yleisesti tekstiä sisältävien tiedostojen, kuten logitiedostojen, nimenmuutosten ja tietokantakyselyiden käsittelyssä.

## Näin

```Bash
# Yksinkertainen esimerkki säännöllisestä lausekkeesta
echo "Tervetuloa Bash-maailmaan!" | grep 'Bash'
```

Tässä koodissa etsimme sanaa "Bash" tekstin joukosta ja tulostamme sen komentoriville. On tärkeää huomata, että grep-komento käyttää säännöllistä lauseketta oletuksena etsinnässä.

```Bash
# Sama esimerkki käyttäen muuttuvaa etsintäkohdetta
sana="Bash"
echo "Tervetuloa Bash-maailmaan!" | grep $sana
```

Voimme myös käyttää muuttujaa säännöllisen lausekkeen määrittelemiseen etsintäprosessissa.

```Bash
# Käyttö regex-substituutiona
echo "Tervetuloa Bash-maailmaan!" | sed 's/Bash/ohjelmointi/g'
```

Sed-komento käyttää säännöllisiä lausekkeita korvaamaan "Bashin" "ohjelmoinniksi" tekstin joukossa.

## Syvällisempää tietoa

Säännöllisillä lausekkeilla on laaja käyttöalue ja niitä on monia erilaisia muotoja. Perusmuotoja ovat yksinkertaiset lausekkeet (basic regular expressions) ja laajennetut lausekkeet (extended regular expressions). Laajennetut lausekkeet sisältävät enemmän toiminnallisuuksia ja ne tunnetaan myös nimellä POSIX-yhteensopivat lausekkeet. Voit oppia lisää näistä muodoista Bashin manuaalisivuilta käyttämällä komentoa `man re_format`.

Säännöllisillä lausekkeilla on myös laaja käyttö eri ohjelmointiympäristöissä. Ne eivät ole sidottuja ainoastaan Bashiin, vaan niitä voi käyttää myös esimerkiksi Pythonissa, Perlissa ja Rubyssa. Näillä kielillä on omat lausekkeisiin liittyvät toimintonsa.

## Katso myös

- [Bashin man-sivut](https://linux.die.net/man/7/regex)
- [Regex Cheatsheet](https://www.rexegg.com/regex-quickstart.html)
- [Lyhyt säännöllisten lausekkeiden opas](https://www.digitalocean.com/community/tutorials/understanding-regex-basics-and-best-practices)