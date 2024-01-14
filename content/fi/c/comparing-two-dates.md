---
title:    "C: Päivämäärien vertaaminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Usein ohjelmoinnissa joudutaan käsittelemään päivämääriä ja vertaamaan niitä keskenään. Tämä voi olla tarpeellista esimerkiksi aikajärjestystä vaativissa sovelluksissa tai tietojen suodattamisessa tietyn ajanjakson sisällä. Vertailu mahdollistaa myös tiettyjen toimintojen suorittamisen tiettyinä aikoina. 

## Kuinka vertailla kahta päivämäärää

Vertailu kahden päivämäärän välillä voidaan tehdä monella eri tavalla, mutta tässä esitellään yksi tapa käyttäen C-ohjelmointikieltä. Käytetään apuna time.h-kirjastoa, joka sisältää valmiita funktioita aikojen käsittelyyn. 

```
#include <stdio.h> 
#include <time.h> 

int main() 
{ 
	struct tm date1; 
	struct tm date2; 
	
	// Asetetaan ensimmäinen päivämäärä 
	date1.tm_year = 2020-1900; 
	date1.tm_mon = 6 - 1; 
	date1.tm_mday = 15; 
	
	// Asetetaan toinen päivämäärä 
	date2.tm_year = 2019-1900; 
	date2.tm_mon = 6 - 1; 
	date2.tm_mday = 15; 
	
	// Muutetaan päivämäärät sekunneiksi 
	time_t time1 = mktime(&date1); 
	time_t time2 = mktime(&date2); 
	
	// Vertaillaan päivämääriä 
	if (difftime(time1, time2) > 0) 
	{ 
		printf("Ensimmäinen päivämäärä on myöhemmin kuin toinen.\n"); 
	} 
	else if (difftime(time1, time2) < 0) 
	{ 
		printf("Toinen päivämäärä on myöhemmin kuin ensimmäinen.\n"); 
	} 
	else
	{ 
		printf("Päivämäärät ovat samat.\n"); 
	} 
	
	return 0; 
} 

```

Esimerkkituloste:

```
Toinen päivämäärä on myöhemmin kuin ensimmäinen.
```

## Syvempi sukellus vertailuun kahden päivämäärän välillä

Muunnos sekunneiksi, kuten edellisessä esimerkissä tehtiin, on yksi mahdollinen tapa vertailla päivämääriä. C-kielessä on kuitenkin tarjolla myös muita hyödyllisiä funktioita päivämäärien käsittelyyn, kuten strftime ja localtime.

Strftime-funktio mahdollistaa päivämäärän muotoilun halutunlaiseksi merkkijonoksi, esimerkiksi viikonpäivä ja kuukauden nimi. Localtime-funktiota käyttäen voidaan hakea myös tarkempia tietoja, kuten viikonpäivä, kuukausi ja vuosiluku.

## Katso myös

- https://www.tutorialspoint.com/c_standard_library/time_h.htm
- https://www.programiz.com/c-programming/library-function/time.h/mktime
- https://www.programiz.com/c-programming/library-function/time.h/localtime
- https://www.cplusplus.com/reference/ctime/tm
- https://www.cs.uic.edu/~jbell/CourseNotes/C_Programming/Time.html

## Katso myös

- https://www.tutorialspoint.com/c_standard_library/time_h.htm
- https://www.programiz.com/c-programming/library-function/time.h/mktime
- https://www.programiz.com/c-programming/library-function/time.h/localtime
- https://www.cplusplus.com/reference/ctime/tm
- https://www.cs.uic.edu/~jbell/CourseNotes/C_Programming/Time.html