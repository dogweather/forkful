---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parseeraa päivämäärä merkkijonosta Bash-ohjelmointikielessä

## Mikä & miksi?
Parseeraus on prosessi, jossa merkkijonosta eristetään tietoja - tässä tapauksessa päivämäärä. Ohjelmoijat tekevät näin, koska joskus päivämäärätiedot saapuvat yleisemmissä muodoissa tai ne voivat syntyä dynaamisesti koodin aikana.

## Näin se tehdään:
```Bash
#!/bin/bash
pvm_merkkijono="2022-05-23"
pvm_objekti=$(date -d$pvm_merkkijono)
echo $pvm_objekti
```
Kun suoritat tämän koodin, tulos näyttää seuraavalta: 
```Bash
Mon May 23 00:00:00 EEST 2022
```

## Syvempi sukellus
Bash tukee päivämärän parseerausta `date`-komennon avulla, joka on osa GNU coreutils-pakettia. Tämä paketti on olennainen osa lähes kaikkia Linux-järjestelmiä.
 
Vaihtoehtona Bashille, käyttäjät voivat käyttää Pythonia tai JavaScriptiä parseeraukseen, joilla on valtavat kirjastot date-tietojen käsittelyyn.
 
Bash 'date'-komanto käsittelee päivämäärät Unix aikaleimoina, jotka ovat sekunteja siitä hetkestä, kun Unix-aika alkoi (1970-01-01 00:00:00 UTC). Tämän tiedon avulla se pystyy tulkitsemaan päivämäärämerkkijonot.

## Katso myös
- [GNU coreutils](https://www.gnu.org/software/coreutils/coreutils.html) 
- [Bash-käsikirjan](https://www.gnu.org/software/bash/manual/bash.html)
- [Python datetime moduuli](https://docs.python.org/3/library/datetime.html)
- [JavaScript Date-objekti](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)