---
title:    "Bash: Merkkijonojen yhdistäminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi yhdistää merkkijonoja ohjelmointia tehdessään? Yhdistäminen tai konkatenointi on tärkeä osa ohjelmointia, sillä se mahdollistaa monimutkaisempien merkkijonojen luomisen yksinkertaisista paloista.

## Kuinka

Yhdistäminen tapahtuu Bashissa käyttämällä oikea- ja vasenpuolista lainausmerkkiä merkkijonojen ympärillä ja plussamerkkiä niiden välissä. Esimerkiksi:

```Bash
string1="Hei"
string2="maailma"
echo $string1$string2 #tulostaa "Heimaailma"
```

Voit myös yhdistää useita merkkijonoja käyttämällä lisäksi toista plussamerkkiä. Esimerkiksi:

```Bash
string1="Tämä on "
string2="hyödyllinen "
string3="artikkeli"
echo $string1$string2$string3 #tulostaa "Tämä on hyödyllinen artikkeli"
```

## Syvässä luotaamisessa

Bashissa yhdistäminen ei rajoitu vain muuttujien väliseen yhdistämiseen. Voit myös yhdistää merkkijonoja funktion palauttamiin arvoihin. Esimerkiksi:

```Bash
function tervehdi() {
  echo "Hei!"
}
tervehdi=" " #huomaa välilyönti
echo $(tervehdi)maailma #tulostaa "Hei maailma"
```

Lisäksi voit käyttää erilaisia erikoismerkkejä, kuten \n uuden rivin luomiseen yhdistettäessä merkkijonoja.

## Katso myös

- [Bashin virallinen opas merkkijonojen käsittelystä](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Opi Bashin perusteet](https://flaviocopes.com/bash-scripting/)
- [Merkkijonojen konkatenointiohjeet eri ohjelmointikielissä](https://www.computerhope.com/issues/ch001721.htm)