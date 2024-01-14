---
title:                "Arduino: Aloittaminen uudesta projektista"
simple_title:         "Aloittaminen uudesta projektista"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan halunnut aloittaa uuden projektin käyttäen Arduinoa, tämä blogi on juuri sinulle! Arduino on monikäyttöinen ja helppokäyttöinen mikrokontrolleri, joka tarjoaa loputtomasti mahdollisuuksia luovuudelle ja insinöörisille haasteille. Mikä tahansa idea, joka sinulla on mielessä, Arduino auttaa sinua toteuttamaan sen.

## Miten

Arduino-ohjelmointi on helppoa ja hauskaa! Aloita lataamalla Arduino IDE ja kytkemällä Arduino mikrokontrolleri tietokoneeseen USB-kaapelilla. Tämän jälkeen voit aloittaa koodaamisen.

```Arduino 
void setup() {
    // Suoritetaan kerran alussa
    pinMode(LED_BUILTIN, OUTPUT); //Asetetaan LED_BUILTIN portiksi
}

void loop() {
    // Suoritetaan loputtomasti
    digitalWrite(LED_BUILTIN, HIGH); //Vaihdetaan LED_BUILTIN tilaa
    delay(1000); //Odota 1 sekunti
    digitalWrite(LED_BUILTIN, LOW); //Vaihdetaan LED_BUILTIN tilaa taas
    delay(1000); //Odota 1 sekunti taas
}
```
Tämä yksinkertainen koodinpätkä saa LED-valon vilkkumaan sekunnin välein. Voit kokeilla muuttaa delay-aikoja saadaksesi erilaisia tuloksia. Voit myös kokeilla muita komentoja, kuten analogisten pinnien lukemista tai käyttää muita antureita ja laitteita. Vain mielikuvitus on rajana!

## Syväsukellus

Jos haluat aloittaa oman projektisi Arduinoa käyttäen, sinun kannattaa aloittaa suunnitteluvaiheesta. Mieti ensin, minkälainen idea sinulla on ja mitä ominaisuuksia tarvitset projektin toteuttamiseen. Seuraavaksi voit piirtää kaavion siitä, miten komponentit kytketään yhteen ja miten Arduino ohjaa niitä. Tämä auttaa hahmottamaan projektisi kokonaisuudessaan ja välttämään virheitä.

Kun suunnitelma on valmis, voit siirtyä koodaamiseen ja testaamiseen. Muista myös dokumentoida projektiasi etenemisen aikana ja opitut asiat, jotta voit helposti palata takaisin ja muokata projektiasi tarvittaessa.

## Katso myös

Tässä muutama hyödyllinen linkki aloittaessa uutta Arduino-projektia:

- [Official Arduino -sivusto](https://www.arduino.cc/)
- [Arduino-alkeiskurssi](https://savsoftacademy.com/courses/coding-from-scratch/)
- [Projektivinkkejä](https://www.thestartuphub.io/nettisivut/john-whitbread/4-bit-led-watch/)
- [Arduino-yhteisö](https://forum.arduino.cc/)
- [Suomenkielinen Arduino-foorumi](https://discourse.arduino.cc/c/international/suomi)