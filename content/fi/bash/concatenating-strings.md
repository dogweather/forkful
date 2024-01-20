---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Merkkijonojen yhdistäminen (string concatenation) tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi. Tämä on tärkeää mm. dynaamisten syötteiden, viestien ja komentojen käsittelyssä.

## Näin käytät:

Bashissa merkkijonojen yhdistäminen on yksinkertaista. 
Katsele seuraavaa koodiesimerkkiä:

```Bash
#!/bin/bash
tervetuloa="Tervetuloa, "
nimi="Pekka"
tervetuloviesti=$tervetuloa$nimi
echo $tervetuloviesti
```

Ja tässä on esimerkin tulostus:

```Bash
Tervetuloa, Pekka
```

## Syvempi Tutkiskelu

Alkuperäisessä Bourne-shellissä merkkijonojen yhdistäminen vaati usein käyttämään erityisiä komentoja tai kikkoja. Bash, joka on Bourne-shelliin perustuva, tekee merkkijonojen yhdistämisestä yksinkertaisempaa.

Yksi vaihtoehto merkkijonojen yhdistämiselle on `printf`-funktion käytöllä. Sen avulla voidaan tulostaa muotoiltu viesti:

```Bash
#!/bin/bash
tervetuloa="Tervetuloa, "
nimi="Pekka"
printf "%s%s" $tervetuloa $nimi
```

Bashissa merkkijonoja voidaan yhdistää myös suoraan ilman välimuuttujaa:

```Bash
#!/bin/bash
nimi="Pekka"
echo "Tervetuloa, $nimi"
```

## Katso Myös

Bash-skriptauksen dokumentaatio: [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)

Bash merkkijonojen käsittely: [The Linux Documentation Project](https://tldp.org/LDP/abs/html/string-manipulation.html)

Bash-skriptauksen jatko-oppimateriaalia: [Awesome Bash](https://awesomeopensource.com/project/awesome-lists/awesome-bash)