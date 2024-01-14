---
title:    "C: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi laskuri-ohjelmiin?

Laskuri-ohjelmat ovat erittäin käteviä työkaluja, kun haluat laskea päivämäärän tulevaisuudessa tai menneisyydessä. Tämä voi auttaa sinua esimerkiksi suunnittelemaan tulevia tapahtumia tai tarkistamaan menneitä tapahtumia. C-ohjelmointikielellä voit luoda omia laskuri-ohjelmia ja lisätä niihin erilaisia toimintoja ja ominaisuuksia haluamallasi tavalla.

## Miten tehdä laskuri-ohjelma C:llä

C-ohjelmointikielellä voit luoda laskuri-ohjelman, joka laskee päivämäärän tietyn määrän päiviä eteenpäin. Tämä voidaan tehdä käyttämällä päivämäärän-aikamoduulia "time.h". Alla on esimerkki koodista, joka laskee päivämäärän 100 päivää eteenpäin ja tulostaa sen muodossa "pp.kk.vvvv".

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Alustetaan tarvittavat muuttujat
  struct tm date = {0}; // Tietue päivämäärälle
  int days_to_add = 100; // Haluttu päivien määrä
  // Asetetaan nykyinen päivämäärä käytettäväksi
  time_t now = time(NULL);
  // Käytetään nykyistä päivämäärää lähtökohtana
  date = *localtime(&now);
  // Lisätään haluttu määrä päiviä eteenpäin
  date.tm_mday += days_to_add;
  // Muutetaan päivämäärä aikaleimaksi
  time_t new_date = mktime(&date);
  // Tulostetaan päivämäärä muodossa "pp.kk.vvvv"
  printf("Päivämäärä 100 päivää eteenpäin: %d.%d.%d\n", date.tm_mday, date.tm_mon + 1, date.tm_year + 1900);
  return 0;
}
```

Tämän esimerkkikoodin tulostus näyttää seuraavalta:

```
Päivämäärä 100 päivää eteenpäin: 19.12.2021
```

## Syvällisempi sukellus päivämäärän laskemiseen

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen voi tuntua monimutkaiselta, mutta C:n "time.h" -moduulin avulla se on melko yksinkertaista. Voit myös lisätä ohjelmaasi muita toimintoja, kuten päivämäärän tarkistamisen tai syötteen kysymisen käyttäjältä.

On myös hyvä huomata, että "time.h" -moduuli käyttää aikaleimoina UNIX-aikaa, joka on sekuntien määrä, joka on kulunut vuodesta 1970 tiettyyn päivämäärään. Siksi laskettu päivämäärä on riippuvainen käyttämäsi laitteen ajasta ja päivämäärä voi olla virheellinen, jos laitteesi aika on asetettu väärin.

## Katso myös

- [C-ohjelmoinnin perusteet](https://fi.wikipedia.org/wiki/C_(ohjelmointikieli))
- [time.h -moduulin dokumentaatio](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Aikaleima ja UNIX-aika](https://fi.wikipedia.org/wiki/Aikaleima)
- [C-ohjelmiin liittyvät artikkelit ja oppaat](https://fi.wikipedia.org/wiki/C_(ohjelmointikieli)#Kirjallisu