---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "C: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuntaa päivämäärän merkkijonoksi? Tämä on hyödyllinen taito, jos haluat tallentaa tai tulostaa päivämäärän eri muodossa tai verrata päivämääriä toisiinsa.

## Näin teet sen

```C
#include <stdio.h>
#include <time.h>

int main()
{
  time_t t = time(NULL);
  struct tm *today = localtime(&t);
  char str_date[20];
  
  // Muunnetaan päivämäärä merkkijonoksi muodossa DD.MM.YYYY
  sprintf(str_date, "%02d.%02d.%04d", today->tm_mday, today->tm_mon + 1, today->tm_year + 1900);
  
  // Tulostetaan muunnettu päivämäärä
  printf("Tänään on %s\n", str_date);
  
  return 0;
}
```
Tämä koodiesimerkki käyttää C:n time.h-kirjastoa ja sen sisältämiä toimintoja muuntaakseen nykyisen päivämäärän merkkijonoksi. Ensiksi time(NULL) hakee ajan kuluneena sekunteina ja tallentaa sen muuttujaan t. Sitten käytetään localtime-toimintoa muuntamaan t-ajan rakenteeksi, josta voimme hakea päivämäärä- ja kuukausiarvon sekä vuosiluvun. Lopuksi sprintf-toiminnolla muodostetaan merkkijono (DD.MM.YYYY) ja tallennetaan se str_date-muuttujaan. Tulostamalla tämän muuttujan, saamme nykyisen päivämäärän merkkijonona.

## Syvempää tietoa

Päivämäärän muuntaminen merkkijonoksi on hyödyllinen taito monessa tilanteessa. C-kielessä tämä tapahtuu usein sprintf-toiminnolla, joka muuntaa erilaisia arvoja merkkijonoiksi. Aikaisemmissa versioissa C:stä, tämä toiminto oli altis ylivuototilanteille ja aiheutti turvallisuusriskejä, mutta nykyisessä C-versiossa oletusarvona on turvallisempi varianssikontrolli, joka sulkee pois nämä riskit.

## Katso myös

- [Aikojen käsittely C:ssä](https://www.sololearn.com/Course/C/)
- [C-sprintf-toiminto](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)
- [C-kielen muotoilumerkinnät](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)