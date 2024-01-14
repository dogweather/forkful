---
title:    "Arduino: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Miksi kenenkään kannattaisi laskea Arduinoilla päivämäärää tulevaisuudessa tai menneisyydessä? Tämä on hyvä kysymys ja vastaus voi olla moninaisia. Jotkut saattavat haluta luoda päiväysnäyttöjä, jotka näyttävät kuluvan päivän tai kuun. Toinen mahdollinen syy on, että halutaan ajastus toimintoja tai mittauksia tiettyihin päivämääriin liittyen. Jotkut saattavat myös yksinkertaisesti olla kiinnostuneita oppimaan uutta Arduino-ohjelmoinnista ja tämä voi olla yksi mielenkiintoinen projekti heille.

## Miten

Laskeminen tietty päivämäärä tulevaisuudessa tai menneisyydessä Arduinoilla vaatii tiettyjä ohjelmointikomentoja ja -lausekkeita. Tässä on esimerkki, miten voit laskea tulevan päivämäärän:

```Arduino
// Määritä nykyinen päivämäärä
int day = 20;
int month = 9;
int year = 2021;

// Määritä haluttu päivämäärä
int futureDay = 1;
int futureMonth = 10;
int futureYear = 2021;

// Muuta päivämäärä millisekunneiksi
unsigned long now = millis(); // nykyinen päivämäärä
unsigned long futureDate = millis() + (futureDay - day) * 86400000 + (futureMonth - month) * 2592000000 + (futureYear - year) * 31536000000UL; // tuleva päivämäärä

// Tulosta tuleva päivämäärä
Serial.print("Tuleva päivämäärä: ");
Serial.print(futureDay);
Serial.print(".");
Serial.print(futureMonth);
Serial.print(".");
Serial.print(futureYear);
Serial.println(".");

// Vertaa päivämääriä ja tulosta viesti sen mukaan
if (futureDate < now) {
  Serial.println("Tuleva päivämäärä on menneisyydessä.");
} else if (futureDate == now) {
  Serial.println("Tuleva päivämäärä on sama kuin nykyinen.");
} else {
  Serial.println("Tuleva päivämäärä on tulevaisuudessa.");
}
```

Tässä koodissa ensin määritetään nykyinen päivämäärä ja sen jälkeen haluttu tuleva päivämäärä. Päivämäärät muutetaan millisekunneiksi ja lasketaan niiden erotus. Lopuksi tulostetaan tuleva päivämäärä ja verrataan sitä nykyiseen päivämäärään. Huomaa, että tämä on yksinkertainen esimerkki ja tosielämässä saattaa olla enemmän muuttujia huomioitavana.

## Syvempi sukellus

Calculating tulevaisuuden tai menneisyyden päivämäärää Arduinoilla voi olla hyödyllistä monissa erilaisissa projekteissa. Esimerkiksi voit käyttää sitä luomaan ajastimia, jotka laukaisevat tiettyjä toimintoja tiettyinä päivämäärinä, tai mittauksia, jotka tallentavat tietoa tiettyinä päivämäärinä. Voit myös käyttää tätä toimintoa luomaan päiväysnäyttöjä, jotka näyttävät kuluvan päivän, kuukauden ja vuoden.

On myös tärkeää huomata, että tämä koodi ei välttämättä toimi kaikissa tilanteissa, jos päivämäärät ovat kaukana toisistaan tai jos kyseessä on karkausvuosi. Tästä syystä