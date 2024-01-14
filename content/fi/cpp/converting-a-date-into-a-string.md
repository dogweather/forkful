---
title:    "C++: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Aina välillä C++ ohjelmoinnissa tulee tarve muuttaa päivämäärä merkkijonoksi. Tämä voi olla tarpeen esimerkiksi laskurin näyttämiseksi käyttäjälle tai tallennettaessa tietoja tietokantaan. Seuraavassa kerromme, kuinka tämä voidaan tehdä helposti ja tehokkaasti.

## Kuinka
Tätä toimintoa varten tarvitsemme käyttöömme muutaman C++ kielisen kirjaston. Ensiksi meidän täytyy sisällyttää <iostream> ja <sstream> kirjastot, koska ne tarjoavat meille tiedon tulostamiseen tarvittavia työkaluja. Tämän lisäksi tarvitsemme myös <ctime> kirjaston, jotta voimme käsitellä päivämäärätietoja. Ohessa on esimerkkikoodi, kuinka voit muuttaa päivämäärän merkkijonoksi.
```C++
#include <iostream>
#include <sstream>
#include <ctime>

int main()
{
	// Luodaan päivämäärä tyyppi time_t
	time_t now = time(0);

	// Muutetaan nyt muuttujan sisältämä päivämäärä merkkijonoksi
	char* date = ctime(&now);

	// Muutetaan merkkijono streamiksi
	std::ostringstream os;

	// Lisätään merkkijono streamiin
	os << "Nykyinen päivämäärä ja aika: " << date;

	// Tulostetaan merkkijonoina
	std::cout << os.str();

	return 0;
}
```
Tämän koodin tulostus näyttää seuraavalta:
```
Nykyinen päivämäärä ja aika: Tue Jul 14 20:25:38 2020
```
Koodi ensin luo time_t tyyppisen muuttujan, johon tallennetaan nykyinen päivämäärä ja aika. Sitten se käyttää ctime() funktiota muuntaakseen tämän muuttujan sisältämän päivämäärän merkkijonoksi. Lopuksi se lisää merkkijonoon myös tarkemman päivämäärän ja tulostaa sen lopulta konsolille.

## Syvempi sukellus
C++ tarjoaa useita erilaisia tapoja muuttaa päivämäärä merkkijonoksi. Esimerkiksi <chrono> kirjastosta löytyy Chrono Type Library, joka tarjoaa C++ standardin mukaiset työkalut aikamääreiden käsittelemiseen. Tämän lisäksi myös boost::date_time kirjasto tarjoaa useita erilaisia päivämäärä ja aika luokkia.

## Katso myös
- <a href="https://www.cplusplus.com/reference/ctime/">ctime()</a> - C++ referenssi päivämäärätietojen käsittelyyn.
- <a href="https://www.cplusplus.com/reference/iomanip/">iomanip</a> - C++ referenssi tavujen tulostamiseen.
- <a href="https://www.boost.org/doc/libs/1_73_0/doc/html/date_time.html">boost::date_time</a> - Boost kirjasto päivämäärä ja aikatietojen käsittelyyn.