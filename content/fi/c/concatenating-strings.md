---
title:                "C: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi käyttää merkkijonojen yhdistämistä? Usein ohjelmoinnin yhteydessä käsitellään useita erillisiä merkkijonoja, ja joskus on tarpeellista yhdistää nämä merkkijonot yhdeksi suuremmaksi kokonaisuudeksi. Tämä voi tapahtua esimerkiksi silloin, kun halutaan luoda tekstipohjainen käyttöliittymä tai tallentaa käyttäjän syöttämät tiedot yhdeksi merkkijonoksi. Tässä blogikirjoituksessa käsittelemme, miten tämä onnistuu kätevästi C-ohjelmointikielessä.

## Miten yhdistetään merkkijonoja C-kielellä

C-kielellä merkkijonojen yhdistäminen tapahtuu kätevästi käyttäen `strcat()`-funktiota. Tämä funktio yhdistää kaksi merkkijonoa toisiinsa lisäämällä toisen merkkijonon perään ensimmäiseen merkkijonoon. Alla on esimerkki siitä, miten `strcat()`-funktiota käytetään:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char nimi[] = "Hanna";
    char sukunimi[] = "Hämäläinen";
    strcat(nimi, sukunimi);
    printf("Koko nimi on: %s", nimi);
    return 0;
}
```

Tämän koodin suorituksen jälkeen `nimi`-merkkijono sisältää arvon "HannaHämäläinen". Jos haluamme lisätä välilyönnin merkkijonojen väliin, voimme käyttää `strcat()`-funktion sijaan `strncat()`-funktiota, joka lisää myös määritetyn määrän merkkejä lähdeviite-merkkijonon perään.

C-kielessä on myös mahdollisuus yhdistää merkkijonoja käyttämällä toisena vaihtoehtona `sprintf()`-funktiota. Tätä funktiota käytettäessä merkkijonossa voi yhdistää myös muita muuttujia, kuten kokonaisluvut, desimaaliluvut tai merkkitietoja. Alla on esimerkki siitä, miten `sprintf()`-funktiota voidaan käyttää:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char kaupunki[] = "Helsinki";
    int vuosi = 2020;
    char kuukausi[] = "heinäkuu";
    char paivamaara[50];
    sprintf(paivamaara, "Tänään on %s %i. %s %i", kaupunki, vuosi, kuukausi, vuosi);
    printf("%s", paivamaara);
    return 0;
}
```

Tämän koodin suorituksen jälkeen `paivamaara`-merkkijonossa on arvo "Tänään on Helsinki 2020. heinäkuu 2020".

## Syvällisempiä tietoja merkkijonojen yhdistämisestä

Merkkijonojen yhdistämisessä on tärkeää huomioida, että lähtö- ja lopputuloksen tulee olla molemmat kelvollisia merkkijonoja. Jos esimerkiksi yhdistettävien merkkijonojen lopusta puuttuu nul-merkki, yhdistämisen tulos ei ehkä ole haluttu. Tämän vuoksi on tärkeää varmistaa, että käytetyt merkkijonot ovat oikean pituisia ennen yhdistämistä.

Samoin tulee huomioida, että käytetyt merkkijonot eivät ylitä kiintiöitä