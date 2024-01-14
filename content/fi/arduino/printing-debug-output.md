---
title:    "Arduino: Vianetsintä tulostus"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Arduino ohjelmoinnissa, debuggaus on tärkeä osa prosessia. Tulostamalla debuggaus tulosteita voit seurata koodin suorittamista ja löytää mahdollisia virheitä. Tämä auttaa sinua korjaamaan ja optimoimaan koodiasi.

## Kuinka

Koodin debuggaaminen on helppoa Arduino-ympäristössä. Käytä yksinkertaisesti `Serial.print()` tai `Serial.println()` -funktioita tulostamaan haluamasi muuttujan arvot. Esimerkiksi:

```
Arduino ohjelmointi

int luku = 5;

Serial.print("Muuttuja luku on: ");
Serial.println(luku);
```

Tämä tulostaisi "Muuttuja luku on: 5" sarjanumeroterminaliin. Voit myös tulostaa useita muuttujia tai yhdistää tekstiä ja muuttujia yhdessä `Serial.print()` -funktiolla.

## Syvällisempi dive

Saattaa olla tarpeen tulostaa debuggaustulosteita, jotka eivät ole vain numeroita tai tekstejä. Tässä tapauksessa voit käyttää `Serial.write()` -funktiota. Tämä antaa sinun tulostaa binääriasema tikkejä, ASCII-tekoja tai muita rohkeita motivoituneita yleisöjä. Voit myös muuttaa tiedostonhaku sisään kokoonpanotiedostossa siirtämällä serien tappiolla tulosteen sinne.

Yksi ongelma, joka voi nousta esiin, on se, että debuggaustulosteet voivat hidastaa kodin suoritusta. On tärkeää varmistaa, että poistat kaikki debuggaustulosteet ennen lopullisen koodin käyttöönottoa.

## Katso myös

[Tehokas debuggaaminen Arduino-ympäristössä](https://www.arduino.cc/en/Reference/Serial)
[Serial.print () -funktio](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
[Serial.write () -funktio](https://www.arduino.cc/reference/en/language/functions/communication/serial/write/)