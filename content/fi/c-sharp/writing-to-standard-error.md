---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "C#: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Kirjoittaminen virheen sijainnin tulosteeseen (engl. writing to standard error) on tapa, jolla ohjelmoijat voivat näyttää virheilmoituksia ohjelman suorituksen aikana. Tämä auttaa ohjelmoijia etsimään ja korjaamaan virheitä koodissaan ja helpottaa ohjelman debuggaamista.

## Miten:

Seuraavassa esimerkissä käytetään C#-kieltä näyttämään virheilmoitus kirjoittamalla se standardi virheen sijaintiin:

```C#
using System;

int x = 5;
int y = 0;
try
{
    int result = x / y;
    Console.WriteLine("Result: " + result);
}
catch (Exception e)
{
    // Kirjoitetaan virheilmoitus standardi virheen sijaintiin
    Console.Error.WriteLine("Virhe: " + e.Message);
}
```

Esimerkin tulosteena näkyy "Virhe: Attempted to divide by zero", mikä kertoo ohjelmoijalle, että laskutoimituksessa on yritetty jakaa nollalla.

## Syväsukellus:

Kirjoittaminen standardi virheen sijaintiin on ollut käytössä jo pitkään ohjelmoinnissa ja se on yleinen tapa koodin debuggaamisessa. Se on myös yksi tapa, jolla ohjelmistoilta saadaan tietoa ongelmista, jota käyttäjät kohtaavat. Toisaalta on myös muita tapoja käsitellä virheilmoituksia, kuten kirjoittaminen standardi tulosteeseen (engl. standard output) tai kirjoittaminen lokitiedostoon (engl. log file).

## Katso myös:

- https://docs.microsoft.com/en-us/dotnet/api/system.console.error
- https://www.dotnetperls.com/console-error
- https://stackoverflow.com/questions/19766396/c-sharp-writing-to-standard-error