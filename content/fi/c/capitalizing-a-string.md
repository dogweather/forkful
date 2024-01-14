---
title:                "C: Merkkijonon suurentaminen"
simple_title:         "Merkkijonon suurentaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluat käsitellä merkkijonoja ja muuttaa niitä isoihin kirjaimiin?

Monissa ohjelmointiprojekteissa on tarvetta muuttaa merkkijonojen kirjainkokoja, esimerkiksi jos haluat tulostaa käyttäjän antaman tekstin isoin kirjaimin tai vertailla kahta merkkijonoa, joissa toinen on isot kirjaimet ja toinen pienet. On myös mahdollista, että käytössäsi on jonkinlainen rajapinta, joka edellyttää merkkijonojen kirjainten muuttamista tiettyyn muotoon.

## Miten: Esimerkkejä ja tulosteita koodilohkojen sisällä

```C
#include <stdio.h>
#include <string.h>

int main()
{
	char string[] = "Hei kaikille!"; //luodaan merkkijono
	int length = strlen(string); //lasketaan merkkijonon pituus
	char new_string[length+1]; //luodaan uusi merkkijono pidentyneelle merkkijonolle
	
	for (int i = 0; i < length; i++) //käydään läpi merkkijono
	{
		if (string[i] >= 'a' && string[i] <= 'z') //tarkistetaan onko kirjain pieni kirjain
		{
			new_string[i] = string[i] - 32; //muunnetaan pieni kirjain isot
		}
		else
		{
			new_string[i] = string[i]; //jos ei ole pieni kirjain, muunnetaan kirjain sellaisenaan
		}
	}
	new_string[length] = '\0'; //lisätään merkkijonon loppuun lopetusmerkki
	printf("%s", new_string); //tulostetaan uusi merkkijono
	
	return 0;
}
```
 
Tulos:

```
HEI KAIKILLE!
```

Toinen esimerkki:

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
	char string[] = "Tämä on tekstinä"; //luodaan merkkijono
	
	for (int i = 0; string[i] != '\0'; ++i) //käydään läpi merkkijono, kunnes tullaan lopetusmerkkiin
  	{
		string[i] = toupper(string[i]); //käytetään toupper-funktiota muuttamaan isot kirjaimet
  	}
  	
  	printf("%s",string); //tulostetaan muutettu merkkijono
  	
  	return 0;
}
```

Tulos:

```
TÄMÄ ON TEKSTINÄ
```

## Syventävä tieto: Lisätietoja merkkijonojen kirjainkoon muuttamisesta

On tärkeää muistaa, että eri ohjelmointikielillä voi olla erilaiset kirjainten käsittelyyn liittyvät funktiot ja kirjoitustavat. Esimerkiksi C-kielessä käytetään kirjainarvojen ASCII-koodauksia, kun taas esimerkiksi Javassa käytetään Unicode-arvoja.

Merkkijonojen kirjainkoon muuttamiseen on myös muita vaihtoehtoisia tapoja, kuten käyttämällä kirjastoja, jotka tarjoavat valmiita funktioita isojen ja pienten kirjainten muuttamiseen. On tärkeää tutkia ja vertailla eri vaihtoehtoja ja valita sopiva tapa projektisi vaatimusten mukaan.

## Katso myös:

- [Ascii-koodaus](https://fi.wikipedia.org/wiki/Ascii)
- [Unicode](https://fi.wikipedia.org/wiki/Unicode)
- [toupper-funktio C-kielessä](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)