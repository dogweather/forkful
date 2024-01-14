---
title:    "C++: Tulevan tai menneen päivämäärän laskeminen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

Laskeminen päivämäärän tulevaisuudessa tai menneisyydessä voi olla hyödyllistä esimerkiksi suunnitellessa loma-aikoja tai tarkistaessa vanhoja tapahtumia.

## Kuinka tehdä

```C++
#include <iostream>
using namespace std;

// Funktio päivämäärän laskemiseen tulevaisuudessa
void laskeTulevaPaiva(int paiva, int kuukausi, int vuosi, int haluttuPaiva) {
	
	// Lisätään haluttu päivämäärä annettuun päivämäärään
	paiva += haluttuPaiva;
	
	// Tarkistetaan, että päivämäärä on oikeassa kuukaudessa
	while (paiva > 30) {
		if (kuukausi == 4 || kuukausi == 6 || kuukausi == 9 || kuukausi == 11) {
			paiva -= 30;
			kuukausi++;
		}
		else {
			paiva -= 31;
			if (kuukausi == 12) {
				kuukausi = 1;
				vuosi++;
			}
			else {
				kuukausi++;
			}
		}
	}
	
	// Tulostetaan tuleva päivämäärä
	cout << paiva << "." << kuukausi << "." << vuosi;
}

int main() {
	// Käyttäjän antama alkuperäinen päivämäärä
	int paiva, kuukausi, vuosi;
	
	cout << "Anna päivämäärä muodossa päivä.kuukausi.vuosi: ";
	cin >> paiva >> kuukausi >> vuosi;
	
	// Käyttäjän haluama tuleva päivämäärä
	int haluttuPaiva;
	cout << "Anna haluttu päivämäärän lisäys: ";
	cin >> haluttuPaiva;
	
	// Kutsutaan laskeTulevaPaiva-funktiota ja annetaan sille parametreina käyttäjän antama päivämäärä ja haluttu päivämäärä
	laskeTulevaPaiva(paiva, kuukausi, vuosi, haluttuPaiva);
	
	return 0;
}
```

**Esimerkki syötteestä ja tulosteesta**:
```
Anna päivämäärä muodossa päivä.kuukausi.vuosi: 28.12.2019
Anna haluttu päivämäärän lisäys: 15
12.1.2020
```

## Syvempää tietoa

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä vaatii tietynlaista logiikkaa, jossa tarkistetaan kuukaudet ja vuodet. Tässä esimerkissä oletetaan, että kaikki kuukaudet ovat 30 päivää pitkiä ja maaliskuu on ensimmäinen kuukausi, jolloin on 31 päivää. Toimivan ohjelman kirjoittamiseksi kannattaa harkita myös lisää erikoistapauksia, kuten karkausvuotta.

## Katso myös

- [https://www.cplusplus.com/forum/articles/80026/](https://www.cplusplus.com/forum/articles/80026/) - ohjeita päivämäärän laskemiseen C++:ssa
- [https://fi.wikipedia.org/wiki/P%C3%A4iv%C3%A4m%C3%A4%C3%A4r%C3%A4](https://fi.wikipedia.org/wiki/P%C3%A4iv%C3%A4m%C3%A4%C3%A4r%C3%A4) - tietoa päivämääristä ja niiden laskemisesta