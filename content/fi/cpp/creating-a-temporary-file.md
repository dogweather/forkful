---
title:    "C++: Väliaikaisen tiedoston luominen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary filejen luominen on tärkeä osa ohjelmointia, sillä se mahdollistaa tiedostojen väliaikaisen tallennuksen ja käytön ohjelmien suorituksen aikana. Temporary filejen käyttö voi myös parantaa ohjelman suoritusaikaa ja muistinkäyttöä verrattuna pysyvien tiedostojen käyttöön.

## Miten

Temporary file voidaan luoda C++:ssa käyttämällä `tmpfile()` -funktiota ja tallentamalla sen palauttama tiedostopointteri muuttujaan. Seuraavassa esimerkissä luodaan temporary file, johon kirjoitetaan "Hello World!" ja lopuksi tiedosto suljetaan ja poistetaan:

```C++
#include <cstdio>

int main() {
    // Temporary file luonti
    FILE* temp = tmpfile();

    // Tiedostoon kirjoittaminen
    fprintf(temp, "%s", "Hello World!");

    // Tiedoston sulkeminen
    fclose(temp);

    // Tiedoston poistaminen
    remove(temp);

    return 0;
}
```

Tämän ohjelman suorituksen jälkeen tiedostoa ei enää löydy järjestelmästä, sillä se oli vain väliaikaisesti käytössä.

## Syvällisempi katsaus

Temporary filet sijaitsevat usein järjestelmän määrittämässä temp-hakemistossa. Näitä tiedostoja ei siis tarvitse erikseen nimetä tai sijoittaa tiettyyn paikkaan. Temporary filejen käyttöä tulee kuitenkin harkita tarkkaan, sillä käyttämätön tai turhaan luotu file voi aiheuttaa epäpuhtauksia järjestelmässä ja hidastaa ohjelman suoritusta. On myös hyvä huomioida, että temporary filejen sisältö voi olla haavoittuvainen ja ne tulee poistaa käytön jälkeen tietoturvasyistä.

## Katso myös

- [Watson Dialog Service](https://github.com/watson-developer-cloud/cpp-sdk)
- [Tempfile Wiki](https://en.wikipedia.org/wiki/Temporary_file)
- [C++ FileIO Tutorial](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)