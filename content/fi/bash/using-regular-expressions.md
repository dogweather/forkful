---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Tulet toimeen päivistä päiviin ilman, että edes huomaat käyttäväsi säännöllisiä lausekkeita. Ne ovat kuitenkin erittäin hyödyllisiä työkaluja koodaajille ja systeemihallinnoijille. Säännölliset lausekkeet, tai regexit, ovat tapa ilmaista monimutkaisia hakuehtoja tekstissä. Niiden avulla voit esimerkiksi hakea tiettyjä merkkijonoja, korvata osia teksteistä tai validoida syötteitä.

## Näin käytät säännöllisiä lausekkeita Bashissa

Bashilla on valmiina sisäänrakennettu tuki säännöllisille lausekkeille. Voit käyttää niitä esimerkiksi grep- ja sed-komentojen kanssa. Alla on muutamia esimerkkejä siitä, miten säännöllisiä lausekkeita voi käyttää Bashissa.

```Bash
# Haku tietystä merkkijonosta
echo "Tämä on Bashin ohjeartikkeli" | grep 'ohje'

# Regex-kirjaimet aakkoset yhdellä kertaa
echo "Bash käyttää regex-sääntöjä." | grep -E 'regex.*sääntö'

# Korvaa tekstiä
echo "Hei maailma!" | sed 's/maailma/kaikki/'

```

Tulostus:
```
Tämä on Bashin ohjeartikkeli
Bash käyttää regex-sääntöjä.
Hei kaikki!
```

Voit myös käyttää regexeja muuttujien ja funktioiden määrittelyssä sekä if-lausekkeiden ehdoissa.

## Syväsukellus

Säännölliset lausekkeet ovat tehokkaita työkaluja, mutta niiden käyttö voi olla aluksi hankalaa. Regexeilla on oma kielioppi, jota täytyy opetella. Yksittäisten merkkien ja merkkiryhmien lisäksi on olemassa myös metakaraktereita, kuten * tai +, jotka ilmaisevat esimerkiksi toiston tai joustavuuden merkkijonossa.

Syvemmälle pääset tutustumalla tarkemmin regexeihin ja niiden ominaisuuksiin. On myös olemassa erilaisia online-työkaluja, joilla voit testata säännöllisiä lausekkeita ja nähdä niiden tulokset reaaliajassa.

## Katso myös

- [Bashin viralliset dokumentaatiot](https://www.gnu.org/software/bash/manual/bash.html)
- [RegExr - regex-testaus- ja -opetusympäristö](https://regexr.com/)
- [Bash20l - interaktiivinen Bash-opetusohjelma](https://www.bash20l.com/)