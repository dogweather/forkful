---
title:    "Bash: Säännöllisten lausekkeiden käyttö"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Bash-ohjelmoinnissa?

Säännölliset lausekkeet ovat voimakas työkalu, jota voidaan käyttää tekstien ja merkkijonojen käsittelyssä Bash-ohjelmoinnissa. Niiden avulla voit suorittaa tehokkaasti monimutkaisia hakuja ja korvauksia, jotka muuten olisivat hankalia tai mahdottomia toteuttaa. Jos haluat parantaa Bash-skriptejäsi ja säästää aikaa ja vaivaa, säännölliset lausekkeet ovat tärkeä taito omaksuttavaksi.

## Kuinka käyttää säännöllisiä lausekkeita Bash-ohjelmoinnissa?

Säännöllisiä lausekkeita käytetään usein yhdessä grep- tai sed-työkalujen kanssa, mutta niitä voidaan myös käyttää suoraan Bash-skripteissä. Alla on muutamia esimerkkejä säännöllisten lausekkeiden käytöstä:

```Bash
# Hae kaikki rivit, jotka sisältävät sana "kissa"
grep "kissa" tiedosto.txt

# Korvaa kaikki "a" kirjaimet "e" kirjaimilla tiedostossa
sed -i 's/a/e/g' tiedosto.txt

# Katso, onko merkkijonossa sana "tervetuloa"
if [[ "$merkkijono" =~ "tervetuloa" ]]; then
    echo "Löydettiin tervetuloa!"
fi
```

Säännöllisiä lausekkeita käytettäessä on tärkeää kiinnittää huomiota oikeaan syntaksiin ja erityisesti erikoismerkkien käsittelyyn. Niiden avulla voit kuitenkin löytää ja korvata monimutkaisia merkkijonoja ja sanoja nopeasti ja tarkasti.

## Syvemmälle säännöllisten lausekkeiden käyttöön

Säännölliset lausekkeet koostuvat erilaisista meta-merkeistä ja tarkennusmerkeistä, jotka mahdollistavat monimutkaisten haku- ja korvaustoimintojen suorittamisen. Näitä ovat mm. sijoitukset (groups), merkinvaihdot (alternations) ja määritykset (quantifiers). Voit oppia lisää näistä käsitteistä ja niiden käytöstä Bashin virallisesta dokumentaatiosta tai muista oppimateriaaleista.

## Katso myös

- [Bash-komentorivi- ja skriptausopas (suomeksi)](https://linux.fi/wiki/Bash-komentorivi-_ja_skriptausopas) 
- [Bashin virallinen dokumentaatio (englanniksi)](https://www.gnu.org/software/bash/manual/bash.html)
- [Regexr - verkkopohjainen säännöllisten lausekkeiden testaus- ja oppimistyökalu (englanniksi)](https://regexr.com/)