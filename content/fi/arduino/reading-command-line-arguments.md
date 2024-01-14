---
title:                "Arduino: Komennoriviparametrien lukeminen"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi: Miksi lukea komentoriviparametreja Arduino-ohjelmoinnissa?

Komentoriviparametrit ovat hyödyllisiä työkaluja, jotka mahdollistavat käyttäjän antaa tarkempia ohjeita ja arvoja ohjelmalle. Tämä voi tehdä ohjelmoinnista joustavampaa ja dynaamisempaa, jolloin voit muokata ohjelmaa tarpeidesi mukaan. Esimerkiksi voit käyttää komentoriviparametreja määrittääksesi tiettyjä arvoja, kuten LED-valojen väriä tai moottorin nopeutta.

## Miten: Esimerkki kuinka lukea komentoriviparametreja Arduino-ohjelmassa

```Arduino
// Alustetaan muuttujat
int arvo1 = 0;
int arvo2 = 0;
// Luetaan komentoriviparametrit
arvo1 = atoi(argv[1]); //Muuntaa ensimmäisen parametrin kokonaisluvuksi
arvo2 = atoi(argv[2]); //Muuntaa toisen parametrin kokonaisluvuksi
// Suoritetaan toimenpiteitä parametrien perusteella
if(arvo1 == 1) {
  // Sytytetään punainen LED-valo
  digitalWrite(LED_PIN, HIGH);
} else if (arvo1 == 2) {
  // Sytytetään vihreä LED-valo
  digitalWrite(LED_PIN, LOW);
}
// Asetetaan moottorin nopeus parametrin mukaan
analogWrite(MOTOR_PIN, arvo2);
```

Käytämme esimerkissä ```atoi()```-funktiota, joka muuntaa merkkijonon kokonaisluvuksi. Tämä mahdollistaa komentoriviparametrien käytön vaihtelevien arvojen antamiseen ohjelmalle. Voit kokeilla muuttaa parametreja ja näet miten ne vaikuttavat ohjelmaan.

## Syvemmälle: Lisätietoa komentoriviparametrien lukemisesta

Komentoriviparametrit syötetään ohjelmalle merkkijonoina ja niitä voidaan käyttää monipuolisesti ohjelmassa. Esimerkiksi voit lisätä useita parametreja ja käyttää niitä eri toimintoihin ohjelmassa. On myös hyvä huomioida, että komentoriviparametrit eivät ole vain Arduinolla käytettävä toiminto, vaan niitä voi käyttää monissa muissakin ohjelmointikielissä.

## Katso myös

- [Atolic - Command line parameters in Arduino](https://atollic.blogspot.com/2011/09/command-line-parameters-in-arduino.html)
- [Arduino Playground - Console Library](https://playground.arduino.cc/Main/console/)
- [Arduino.cc - int()](https://www.arduino.cc/reference/en/language/functions/communication/serial/int/)