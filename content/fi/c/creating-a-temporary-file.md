---
title:    "C: Väliaikaisen tiedoston luominen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda tilapäinen tiedosto?

Tilapäisten tiedostojen luominen on tärkeä osa monia ohjelmointitehtäviä. Ne tarjoavat lyhytaikaisen tallennustilan tietojen käsittelyyn ja välittämiseen. Tilapäisten tiedostojen käyttö voi myös olla hyödyllistä silloin, kun halutaan välttää pysyvien tiedostojen luomista.

## Miten luoda tilapäinen tiedosto?

Tilapäisen tiedoston luominen C-kielellä on hyvin yksinkertaista. Se vaatii vain muutaman rivin koodia, kuten esimerkiksi:

```C
// Avataan uusi tiedosto nimellä "temporary.txt" luomistilassa, joka mahdollistaa tiedoston kirjoittamisen ja lukemisen
FILE *tempFile = fopen("temporary.txt", "w+");

// Tarkistetaan, että tiedoston avaaminen onnistuu
if (tempFile == NULL) {
    // Jos ei onnistu, tulostetaan virheilmoitus ja lopetetaan ohjelma
    printf("Tilapäistiedoston luominen epäonnistui.\n");
    exit(1);
}

// Kirjoitetaan tiedostoon sisältöä
fprintf(tempFile, "Tämä on esimerkkiteksti tilapäisessä tiedostossa.");

// Suljetaan tiedosto
fclose(tempFile);
```

Koodia suorittaessa voidaan huomata, että uusi tiedosto nimeltä "temporary.txt" on luotu ja siihen on kirjoitettu teksti. Tämän jälkeen tiedosto suljetaan ja poistetaan käytöstä.

## Syvällisempää tietoa tilapäisen tiedoston luomisesta

Tilapäisen tiedoston luominen tapahtuu usein kahdessa vaiheessa. Ensimmäisessä vaiheessa avataan uusi tiedosto ja varmistetaan, että tiedoston avaaminen onnistuu. Toisessa vaiheessa tiedostoon kirjoitetaan sisältöä ja suljetaan tiedosto. Tämän jälkeen tiedosto voidaan poistaa käytöstä.

Tilapäisten tiedostojen käyttö on erityisen hyödyllistä silloin, kun ohjelmassa käsitellään suuria määriä dataa tai tarvitaan väliaikainen säilytyspaikka tiedon lähettämiseen ja vastaanottamiseen. Ne myös auttavat vähentämään pysyvien tiedostojen määrää, mikä voi olla hyödyllistä esimerkiksi turvallisuuden kannalta.

## Katso myös

- [Tilapäisen tiedoston luominen C-kielellä](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [C-ohjelmoinnin perusteet](https://fi.wikibooks.org/wiki/C/_ohjelmoinnin_perusteet)