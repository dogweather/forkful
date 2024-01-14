---
title:                "Bash: Jonon yhdistäminen"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisin yhdistää merkkijonoja? Merkkijonojen yhdistäminen on hyödyllistä, kun halutaan luoda dynaamisia tekstejä esimerkiksi skriptien tai ohjelmien avulla.

## Miten

```Bash
# Luodaan muuttuja "nimi" ja sijoitetaan siihen teksti "Maailma"
nimi="Maailma"
# Käytetään yhdistämiseen merkkiä "+"
tervehdys="Hei " + $nimi
# Tulostetaan tervehdys
echo $tervehdys
```

Tulostus:
```
Hei Maailma
```

Merkkijonojen yhdistämiseen Bash-ohjelmoinnissa käytetään usein kahta erilaista merkkiä: kahta lainausmerkkiä (") ja plus-merkkiä (+). Lainausmerkit ympäröivät yhdistettävät merkkijonot ja plus-merkki yhdistää ne toisiinsa.

Jos haluat yhdistää useampia merkkijonoja, voit käyttää useampia plus-merkkejä tai yksinkertaisesti kirjoittaa merkkijonot peräkkäin ilman merkkejä välissä.

## Syväsukellus

Merkkijonojen yhdistäminen voi myös olla hyödyllistä, kun halutaan luoda monimutkaisempia tekstejä esimerkiksi käyttäjän syötteen tai järjestelmän muuttujien avulla. Bash-ohjelmointikielen sisäänrakennetut muuttujat, kuten $USER, voivat helpottaa tekstien luomista ja yhdistämistä.

Lisäksi Bashilla on joitakin käteviä komentoja, kuten "echo", jotka tekevät merkkijonojen yhdistämisestä entistä helpompaa ja nopeampaa.

Merkkijonojen yhdistäminen on myös hyödyllistä silloin, kun halutaan luoda dynaamisia komentoja, jotka pohjautuvat erilaisiin muuttujiin tai käyttäjän syötteeseen.

## Katso myös

- [Merkkijonon yhdistäminen Pythonilla](https://www.digitalocean.com/community/tutorials/how-to-use-string-concatenation-in-python-3)
- [Bash-ohjelmoinnin perusteet](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Käyttäjän syötteen lukeminen Bashilla](https://linuxize.com/post/bash-read/)