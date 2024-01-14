---
title:    "Arduino: Kahden päivämäärän vertailu"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Monissa Arduino projekteissa on tarve vertailla kahta päivämäärää, kuten tarkistaa onko tietty päivämäärä määritetty ajankohtaan asti tai vertailla kahta päivämäärää toisiinsa. Tämä on erityisen hyödyllistä esimerkiksi lämpötila- tai kosteusmittauksissa, kun halutaan tietää, miten tietyt olosuhteet ovat muuttuneet tietyn ajanjakson aikana.

## Näin teet sen

Arduino tarjoaa joitain hyödyllisiä funktioita päivämäärien vertailua varten. Tässä on esimerkki siitä, kuinka voit vertailla kahta päivämäärää ja tulostaa tietoja sarjaporttiin:

```Arduino
#include <DateTime.h>
#include <DateTimeStrings.h>

void setup() {
  Serial.begin(9600);
  DateTime date1(2021, 3, 15, 12, 0, 0); // ensimmäinen päivämäärä
  DateTime date2(2021, 3, 20, 14, 0, 0); // toinen päivämäärä
  // tulostetaan ensimmäinen päivämäärä
  Serial.print("Ensimmäinen päivämäärä: ");
  Serial.println(dateTimeToString(date1));
  // tulostetaan toinen päivämäärä
  Serial.print("Toinen päivämäärä: ");
  Serial.println(dateTimeToString(date2));
  // verrataan päivämääriä
  if (date1 < date2) {
    Serial.println("Ensimmäinen päivämäärä on aikaisempi");
  } else if (date1 > date2) {
    Serial.println("Toinen päivämäärä on aikaisempi");
  } else {
    Serial.println("Päivämäärät ovat samat");
  }
}

void loop() {

}
```

Tässä esimerkissä käytämme Arduino DateTime-kirjastoa ja sen funktion `dateTimeToString()` avulla saamme päivämäärän tulostettua sarjaporttiin. Vertailu tehdään käyttämällä vähemmän tai suurempaa operaattoria, joka vertailee kahta päivämäärää toisiinsa.

Esimerkkitulostus sarjaporttiin:

```
Ensimmäinen päivämäärä: mar 15. 2021 12:00:00
Toinen päivämäärä: mar 20. 2021 14:00:00
Ensimmäinen päivämäärä on aikaisempi
```

Nyt voit soveltaa tätä esimerkkiä omiin projekteihisi ja verrata päivämääriä haluamallasi tavalla.

## Syvenny aiheeseen

Päivämäärien vertailu ja laskeminen voi olla haastavaa, sillä päivämäärät koostuvat monista osista, kuten päivästä, kuukaudesta, vuodesta ja jopa ajasta. On tärkeää muistaa, että päivämäärät tulee aina esittää oikeassa järjestyksessä, sillä muuten tulos voi olla virheellinen.

Voit myös käyttää muita funktioita kuten `second()`, `minute()`, `hour()`, `day()`, `month()` ja `year()` saadaksesi tarkempia tietoja päivämääristä ja vertailla niitä haluamallasi tavalla.

## Katso myös

- [DateTime library reference](https://www.arduino.cc/reference/en/libraries/datetime/)
- [DateTime comparison operators](https://www.arduino.cc/reference/en/libraries/datetime/comparison-operators/)
- [DateTimeStrings library reference](https://www.arduino.cc/reference/en/libraries/datetimestrings/)