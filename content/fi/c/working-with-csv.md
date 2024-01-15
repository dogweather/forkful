---
title:                "Työskentely csv-tiedostojen kanssa"
html_title:           "C: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

Monet sovellukset ja ohjelmistot käyttävät CSV (Comma Separated Values) tiedostomuotoa tallentaakseen ja jakaa dataa. CSV on myös ihanteellinen muoto käsitellä taulukoita ja suuria määriä dataa. Siksi on tärkeää osata työskennellä CSV-tiedostojen kanssa C-ohjelmoinnissa. 

## Miten

CSV-tiedostojen käsittely C-kielellä on melko yksinkertaista ja nopeaa. Suurin osa toiminnoista vaatii vain muutaman koodirivin. Alla on muutamia esimerkkejä CSV-tiedostojen lukuun ja kirjoitukseen käyttäen C-koodia:

### Lue CSV-tiedosto

```
FILE *file = fopen("tiedostonimi.csv", "r"); // Avataan tiedosto lukureetta
if(file == NULL) { // Tarkistetaan, että tiedosto aukeaa
  printf("Tiedoston avaus epäonnistui.");
  return 1;
}

char line[256]; // Alustetaan taulukko riville
while(fgets(line, sizeof(line), file)) { // Luetaan tiedoston kaikki rivit
  char *token; // Määritetään muuttuja erotellun merkkijonon palauttamiseksi
  token = strtok(line, ","); // Erotellaan rivi pilkulla
  while(token != NULL) { // Käydään läpi kaikki erotellut merkkijonot
    printf("%s\n", token); // Tulostetaan jokainen muuttuja omalle riville
    token = strtok(NULL, ","); // Erotetaan seuraava merkkijono
  }
}
fclose(file); // Suljetaan tiedosto
```

Esimerkki CSV-tiedostosta:

```
Name, Age, City
John, 25, Helsinki
Lisa, 32, Tampere
```

Output:

```
Name
Age
City
John
25
Helsinki
Lisa
32
Tampere
```

### Kirjoita CSV-tiedosto

```
FILE *file = fopen("uusi_tiedosto.csv", "w"); // Avataan tiedosto kirjoitustilassa
fprintf(file, "Name, Age, City\n"); // Lisätään ensimmäinen rivi otsikkoriviksi
fprintf(file, "John, 25, Helsinki\n"); // Lisätään tietoja riveille
fprintf(file, "Lisa, 32, Tampere\n");
fclose(file); // Suljetaan tiedosto
```

### Lisää rivi CSV-tiedostoon

```
FILE *file = fopen("tiedostonimi.csv", "a"); // Avataan tiedosto lisäystilassa
fprintf(file, "Bob, 40, Oulu\n"); // Lisätään uusi rivi
fclose(file); // Suljetaan tiedosto
```

Muista, että CSV-tiedostossa jokainen rivi tulee olla samassa muodossa ja eroteltu samalla merkillä, yleisimmin pilkulla.

## Deep Dive

Tässä osiossa sukellamme hieman syvemmälle CSV-tiedostojen maailmaan. Yksi hyödyllinen työkalu CSV-tiedostojen käsittelyssä C-kielellä on "csv" kirjasto, jonka avulla voit helposti lukea, kirjoittaa ja muokata CSV-tiedostoja.

Toinen tärkeä asia muistaa on tiedostojen käsittelyn oikeaoppinen lopettaminen. Muista aina sulkea tiedostot sen jälkeen kun olet lopettanut niiden käsittelyn, jotta vältät mahdolliset ongelmat.

See Also: 

- [C CSV kirjasto](https://github.com/ryanwoodsmall/c-csv)
- [Official C Documentation](https://www.gnu.org/software/gsl/doc/html/csv.html)