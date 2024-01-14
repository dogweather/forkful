---
title:    "Arduino: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluat muuntaa päivämäärän merkkijonoksi? Päivämäärän muuntaminen merkkijonoksi on hyödyllinen tekniikka, jota voit käyttää esimerkiksi tekstinäytöllä tai tallennettaessa tietokantaan. Tämä antaa sinulle mahdollisuuden näyttää päivämäärän haluamassasi muodossa ja käsitellä sitä helpommin.

## Kuinka

Merkkijonojen käyttäminen Arduino-ohjelmoinnissa voi vaikuttaa monimutkaiselta, mutta se on itse asiassa varsin helppoa. Sinun tarvitsee vain käyttää time.h-kirjastoa ja sen sisältämiä valmiita funktioita. Alla on esimerkki, joka näyttää, kuinka muuntaa nykyinen päivämäärä merkkijonoksi:

```Arduino
#include <Time.h>

void setup() {
  // Alustetaan sarjaportti
  Serial.begin(9600);
}

void loop() {
  // Haetaan nykyinen aika
  time_t nyt = now();

  // Muutetaan aika merkkijonoksi
  String paivamaara = String(day(nyt)) + "." + String(month(nyt)) + "." + String(year(nyt));

  // Tulostetaan merkkijono sarjaporttiin
  Serial.println(paivamaara);

  // Odota 1 sekunti
  delay(1000);
}
```

Yllä olevassa koodissa käytetään valmiita day(), month() ja year() funktioita, jotka palauttavat päivän, kuukauden ja vuoden numeroina. Ne yhdistetään String-funktiolla ja tulostetaan sarjaporttiin.

**Output:**

```
26.9.2021
```

## Syvällinen tarkastelu

Arduino-ohjelmointiympäristö tarjoaa myös muita valmiita funktioita, kuten hour(), minute() ja second(), joilla voit hakea tarkempaa aikaa. Merkkijonon muuntamisen lisäksi voit myös muuntaa ajan millisekunneiksi käyttämällä millis() funktiota.

Voit myös käyttää erilaisia muotoilumerkkejä muotoillaksesi päivämäärän haluamallasi tavalla. Esimerkiksi voit lisätä merkinnän "ti" päivän jälkeen käyttämällä merkkiä "%a" tai lisätä etunollan numeroiden eteen käyttämällä merkkiä "%02d". Täydellinen lista kaikista muotoilumerkeistä löytyy täältä: http://www.cplusplus.com/reference/ctime/strftime/

## Katso myös

- Arduino String-tietotyyppi: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Millis() funktio: https://www.arduino.cc/reference/en/language/functions/time/millis/
- Muotoilumerkit: http://www.cplusplus.com/reference/ctime/strftime/