---
title:    "C: Kaavan mukaiseen merkkijonon poistaminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa on tarpeen poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi olla erittäin hyödyllistä esimerkiksi tekstimuotoilussa tai tietojen käsittelyssä.

## Kuinka tehdä

C-kielen avulla tämän tavoitteen saavuttaminen voi olla hyvin yksinkertaista. Alla olevassa koodiesimerkissä näet, kuinka käytännössä voidaan poistaa kaikki numerot sisältävä merkkijono. Tulostuksena saat puhtaan merkkijonon ilman numeroita.

```C
#include <stdio.h>
#include <string.h>

int main()
{
	char string[] = "Tämä on esimerkkimerkkijono, johon on lisätty 1234 numeroita.";
	char result[100];

	int i, j = 0;

	for(i = 0; i < strlen(string); i++)
	{
		if(!isdigit(string[i]))
		{
			result[j] = string[i];
			j++;
		}
	}

	result[j] = '\0';
	printf("Tulos: %s", result);

	return 0;
}
```

Tulostus:

```
Tulos: Tämä on esimerkkimerkkijono, johon on lisätty numeroita.
```

## Syvemmälle

Voidaksemme ymmärtää tarkemmin, kuinka merkkejä voi poistaa C-kielellä, on hyödyllistä tietää joitakin perustietoja merkkijonoista ja niiden käsittelystä. C-kielessä merkkijonot ovat taulukoita, joissa jokainen merkki vastaa yhtä taulukon alkiota. Merkkien poistaminen tapahtuu siis yksinkertaisesti siten, että poistettavat merkit jätetään huomioimatta ja loput merkit siirretään taulukossa siihen kohtaan, josta poistettu merkki alkoi.

## Katso myös

- [The C Programming Language](https://en.wikipedia.org/wiki/The_C_Programming_Language)
- [C - Merkkijono](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Regex-tutoriaali](https://regexone.com/)