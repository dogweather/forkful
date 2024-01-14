---
title:    "C: Merkkijonon suuruuden muuttaminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmissa tarvitaan muuttaa merkkijonojen kirjainten kokoja, esimerkiksi otsikoita tai tietuetta varten. Tämä on yleistä esimerkiksi käyttäjän syötteen validoinnissa, jossa syötettyä merkkijonoa halutaan verrata johonkin tiettyyn merkkijonoon, mutta kirjainkoolla ei ole merkitystä. Tässä tapauksessa tarvitaan funktion, joka kykenee muuttamaan merkkijonon kirjainten kokoja.

## Miten se tehdään

C-kielen standardikirjastossa on valmiina funktio `toupper()`, joka muuttaa annetun merkkijonon kirjaimet isoiksi kirjaimiksi. Tämä funktio ottaa parametrinaan yhden merkin ja palauttaa sen isoilla kirjaimilla. Käytännössä tämä tarkoittaa sitä, että funktioon tulee antaa jokainen merkki vuorollaan, jotta koko merkkijono muuttuu halutun muotoiseksi.

```
#include <stdio.h>
#include <ctype.h>

int main() {
    char string[] = "tämä on tekstiä";
    for (int i = 0; string[i] != '\0'; i++) {
        string[i] = toupper(string[i]);
    }

    printf("%s", string); // Tulostaa "TÄMÄ ON TEKSTIÄ"
    return 0;
}
```

Yllä olevassa esimerkissä käytetään `for`-silmukkaa ja `toupper()`-funktiota muuttamaan jokainen merkki isoiksi kirjaimiksi. Samaa voidaan käyttää myös `while`-silmukan kanssa ja käydä läpi merkkijonoa kunnes koko merkkijono on käyty läpi.

## Syvällinen tarkastelu

C-kielessä merkkijonot ovat taulukoita, eli ne koostuvat merkeistä, joilla jokaisella on oma indeksinsä. Tästä syystä merkkijonon kirjainten muuttaminen on samanlainen prosessi kuin taulukoiden käsittely. `toupper()`-funktio hyödyntää tavallista ASCII-taulukkoa, jossa jokaiselle pienelle kirjaimelle on vastaava iso kirjain.

On myös hyvä tiedostaa, että `toupper()`-funktio ei toimi skandinaavisille kirjaimille tai muille erikoismerkeille, sillä ASCII-taulukko ei sisällä näitä merkkejä. Tällöin joudutaan käyttämään muita kirjastofunktioita tai itse kirjoittamaan oma funktio muuttamaan kirjaimia halutulla tavalla.

## Katso myös

- [C-kie