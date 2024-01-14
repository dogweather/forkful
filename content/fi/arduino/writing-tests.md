---
title:    "Arduino: Testien kirjoittaminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa testeja Arduino-ohjelmointiin?

Testien kirjoittaminen Arduino-ohjelmointiin voi vaikuttaa turhalta ja aikaa vievältä askareelta, mutta se voi todella auttaa välttämään monia ongelmia ja virheitä ohjelmoinnissa. Testit voivat myös parantaa koodin luotettavuutta ja helpottaa sen ylläpitämistä.

## Näin kirjoitat testeja - esimerkki

```Arduino
// Testattavat funktiot
int summa(int a, int b) {
  int tulos = a + b;
  return tulos;
}

int vahenna(int a, int b) {
  int tulos = a - b;
  return tulos;
}

// Testit
void test_summa() {
  int tulos = summa(2, 3);
  if (tulos == 5) {
    Serial.println("Summa-testi läpäisi!");
  } else {
    Serial.println("Summa-testi epäonnistui! Odotettiin 5, saatiin " + String(tulos));
  }
}

void test_vahenna() {
  int tulos = vahenna(5, 2);
  if (tulos == 3) {
    Serial.println("Vähennä-testi läpäisi!");
  } else {
    Serial.println("Vähennä-testi epäonnistui! Odotettiin 3, saatiin " + String(tulos));
  }
}

// Aja testit setup-funktion sisällä
void setup() {
  Serial.begin(9600);
  
  test_summa();
  test_vahenna();
}

void loop() {

}
```

Kun koodi ajetaan, tulostuu Sarjaporttiin seuraava:

```
Summa-testi läpäisi!
Vähennä-testi läpäisi!
```

Tämä tarkoittaa, että molemmat testit suoritettiin oikein ja testattavat funktiot toimivat odotetulla tavalla.

## Syvällisempi sukellus testien kirjoittamiseen

Testien kirjoittamisen avulla voidaan varmistaa, että koodi toimii oikein eri tilanteissa ja erilaisten syötteiden kanssa. Se auttaa myös tarkistamaan, että koodi toimii myös muutosten jälkeen ja mahdollisesti aiheuttamatta uusia virheitä.

Testien kirjoittamisen yhteydessä kannattaa miettiä, mitä kaikkia tapauksia halutaan testata ja mitä odotetaan jokaiselta testitulokselta. Testikattavuuden avulla voidaan varmistaa, että kaikki mahdolliset tapaukset on testattu ainakin kerran.

Hyvä käytäntö on myös kirjoittaa testejä ennen varsinaisen koodin kirjoittamista. Tämä auttaa suunnittelemaan ja hahmottamaan koodia paremmin ja voi säästää aikaa ja vaivaa myöhemmin.

## Katso myös

* [Arduinon testaussivusto](https://www.arduino.cc/en/Guide/Arduinouno)
* [Johdanto testiautomaatioon Arduino-ohjelmoinnissa](https://learn.sparkfun.com/tutorials/testing-arduino-code-in-the-arduino-ide/all)
* [Testauskirjastot Arduinolle](https://github.com/Testato/Testing?_pjax=%23js-repo-pjax-container#unit-testing-libraries)