---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "C: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoitus virheilmoituksiin eri ohjelmointikielillä voi olla hyödyllistä esimerkiksi virheiden vianetsinnässä tai ohjelman kehityksen aikana. C-kielellä virheiden tulostaminen standardi virhevirtaan (standard error) on tärkeä osa ohjelmointia.

## Kuinka

Käyttämällä "fprintf" -funktiota voidaan kirjoittaa tekstiä standardi virhevirtaan. Esimerkiksi:

```C
fprintf(stderr, "Tämä on esimerkki virheilmoituksesta!");
```

Tämä tulostaa tekstin "Tämä on esimerkki virheilmoituksesta!" standardi virhevirtaan. Voit myös käyttää "perror" -funktiota, joka tulostaa viestin, joka vastaa tiettyä virhenumeroa. Esimerkiksi:

```C
perror("Tiedostoa ei löydy!");
```

tulostaisi tekstin "Tiedostoa ei löydy!: No such file or directory" standardi virhevirtaan.

## Syväsukellus

Standardi virhevirta (stderr) on yksi kolmesta vakiotiedostovirrasta C-kielessä. Se on tarkoitettu virheilmoitusten ja diagnostiikkatietojen tulostamiseen ohjelmassa. Sen avulla voidaan helposti erottaa normaali tulostus (stdout) ja virheilmoitukset. Voit myös ohjata standardi virhevirran haluamaasi tiedostoon, esimerkiksi virhelokiin, käyttämällä "freopen" -funktiota. Tämä voi olla hyödyllistä esimerkiksi tuotannossa olevassa ohjelmassa.

## Katso myös

- https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm
- https://www.tutorialspoint.com/c_standard_library/c_function_perror.htm