---
title:                "C: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

Monissa ohjelmoinnin projekteissa saattaa tulla tarve luoda väliaikaisia tiedostoja. Tämä voi tapahtua esimerkiksi silloin, kun ohjelma tarvitsee tallentaa väliaikaista dataa muistiin ja käsitellä sitä myöhemmin, tai kun yhteyksiä ulkoisiin resursseihin tulee luoda ja lukea tiedostoja. Tässä blogipostissa käymme läpi, kuinka voit luoda väliaikaisia tiedostoja C-ohjelmointikielellä.

## Näin luot väliaikaisen tiedoston

C-ohjelmointikielessä on olemassa useita tapoja luoda väliaikaisia tiedostoja, mutta yleisin ja suositeltavin tapa on käyttää standardikirjaston `tmpfile()`-funktiota. Tämän funktion avulla voit luoda väliaikaisen tiedoston järjestelmän oletuskansioon ja käyttää sitä kuten tavallista tiedostoa. Ole kuitenkin varovainen, sillä nämä tiedostot tuhotaan automaattisesti ohjelman sulkeutuessa.

Tämän seuraavan koodiesimerkin avulla voit luoda väliaikaisen tiedoston ja kirjoittaa siihen tekstirivin:

```C
#include <stdio.h> 

int main() 
{
    FILE *tf = tmpfile(); // luodaan väliaikainen tiedosto
    fputs("Tämä on väliaikainen tiedosto.", tf); // kirjoitetaan tiedostoon rivi
    fclose(tf); // suljetaan tiedosto
    return 0;
}
```

Ohjelman suorituksen jälkeen voit tarkistaa oman järjestelmäsi väliaikaisten tiedostojen kansiosta, ja näet siellä uuden nimettömän tiedoston, johon koodiesimerkissä kirjoitettu teksti on tallentunut.

## Syvemmälle väliaikaisten tiedostojen luomiseen

Väliaikaisten tiedostojen luominen voi vaikuttaa helpolta, mutta on tärkeää ymmärtää, että nämä tiedostot ovat rajallisia resursseja. Järjestelmäsi väliaikaisten tiedostojen kansioon on määritetty ennalta sallittu määrä nykyisiä tiedostoja, ja jos ylität tämän määrän, väliaikainen tiedosto ei enää luoda. Ole siis tietoinen siitä, kuinka monta väliaikaista tiedostoa luot ja varmista, että suljet ne kaikki ohjelman suorituksen jälkeen.

## Katso myös

Tässä blogipostissa olemme keskittyneet vain yhteen tapaan luoda väliaikaisia tiedostoja C-ohjelmointikielessä. Voit kuitenkin syventää tietämystäsi ja tutustua muihin tapoihin ja mahdollisiin sudenkuoppiin lukemalla seuraavat artikkelit:

- C `tmpfile()`-funktio: https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm
- Väliaikaiset tiedostot Unix-ympäristössä: https://www.linkedin.com/pulse/you-really-know-anything-temporary-file-jeremy-ross-smith/
- C-kielen tiedostonkäsittely: https://www.cprogramming.com/tutorial/cfileio.html