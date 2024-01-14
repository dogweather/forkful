---
title:    "C: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa tutustumaan C-ohjelmointikieleen ja siihen, miten päivämäärä voidaan muuttaa merkkijonoksi. Tämä taito on hyödyllinen monissa ohjelmointiprojekteissa, ja voit käyttää sitä luomaan erilaisia toimintoja, kuten kalentereita tai aikaleimoja.

## Miten tehdä

Aloitetaan esimerkillä, joka muuttaa päivämäärän merkkijonoksi. Tarvitsemme päivämäärän tallennettuna päivä, kuukausi ja vuosi -muuttujiin, ja käytämme sprintf-funktiota muuntamaan arvot merkkijonoksi.

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
   int day, month, year;
   char date[12];
   
   printf("Anna päivämäärä muodossa dd/mm/yyyy: ");
   scanf("%d/%d/%d", &day, &month, &year);
   
   sprintf(date, "%02d/%02d/%04d", day, month, year);
   // Lukuja esittävät %d merkinnät 
   // muunnetaan merkkijonoiksi käyttämällä %02d ja %04d 
   
   printf("Päivämäärä merkkijonona: %s", date);
   
   return 0;
}
```

Output:
```
Anna päivämäärä muodossa dd/mm/yyyy: 12/05/2020
Päivämäärä merkkijonona: 12/05/2020
```

Seuraavaksi katsotaan esimerkki, joka käyttää strftime-funktiota muuttamaan päivämäärän merkkijonoksi annetun formaatin mukaan. Tässä tapauksessa käytämme myös ajan tietoja, joten sisällytämme tietyt kirjastot.

```C
#include <stdio.h>
#include <time.h>

int main()
{
   time_t now;
   struct tm *timeinfo;
   char date[50];
   
   time(&now);
   timeinfo = localtime(&now);
   
   strftime(date, 50, "Tänään on %d.%m.%Y klo %H:%M:%S", timeinfo);
   // %d edustaa päivää, %m kuukautta, %Y vuotta, %H tuntia, %M minuutteja, %S sekunteja
   
   printf("%s", date);
   
   return 0;
}
```
Output:
```
Tänään on 12.05.2020 klo 13:25:43
```

## Syvemmälle

Päivämäärän muuntaminen merkkijonoksi voi aluksi vaikuttaa hankalalta ja monimutkaiselta, mutta onneksi C-kielessä on useita hyödyllisiä funktioita ja kirjastoja, jotka helpottavat tätä tehtävää. sprintf-funktio ottaa argumenteiksi merkkijonon ja numeromuuttujia ja muuntaa ne halutun muotoon, kun taas strftime hyödyntää aikatietoja ja antaa mahdollisuuden valita haluamansa formaatin.

Olisi myös hyvä tutustua erilaisiin aikafunktioihin C-kielessä ja niiden käyttöön. Näiden avulla voit tehdä tarkempia ja monipuolisempia toimintoja päivämäärän käsittelyssä.

## Katso myös

- [C-kieli - Ohjelmointipaketti](https://www.cs.cmu.edu/~ab/15-123S11/Lectures/Lecture%2003.pdf)
- [sprintf-funktion dokumentaatio](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html)
- [strftime-funktion dokumentaatio](https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html#Low_002dLevel-Time-String-Parsing)