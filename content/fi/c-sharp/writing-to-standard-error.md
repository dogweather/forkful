---
title:    "C#: Standardivirheeseen kirjoittaminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit standard erroriin? Siihen on kaksi pääsyytä: ensinnäkin, se on kätevä tapa havaita ja korjata ohjelmistovirheitä. Toiseksi, se mahdollistaa käyttäjäystävällisemmän käyttöliittymän, koska virheilmoitukset voidaan näyttää tekstiplotussa, eikä vain konsolissa.

## Miten

Standard erroriin kirjoittaminen C#:ssä on melko yksinkertaista. Käytämme siihen Console-luokan WriteLine-metodia ja ohjaamme tulosteen StandardError-virrankäsittelyyn. Seuraava esimerkkikoodi näyttää tämän käytännössä:

```C#
Console.SetError(new StreamWriter(Console.OpenStandardError()));
Console.SetError(new StreamWriter(Console.OpenStandardError(), Encoding.UTF8));
Console.Error.WriteLine("Tämä on virheilmoitus, joka kirjoitetaan standard erroriin!");
```

Tämän tuloksena saamme seuraavan virheilmoituksen:

```
Tämä on virheilmoitus, joka kirjoitetaan standard erroriin!
```

## Syväsukellus

Kun kirjoitamme standard erroriin, meidän pitäisi myös ottaa huomioon muutama asia. Ensinnäkin, standard erroriin kirjoitetut tiedot ovat näkyvissä vain siinä prosessissa, jossa ne on kirjoitettu. Toiseksi, koska se on virrankäsittely, se on suunniteltu käsittelemään tekstiä, joten emme voi kirjoittaa siihen muita tietotyyppejä, kuten numeroita tai boolean-arvoja.

## Katso myös

- [Microsoftin dokumentaatio kirjoittamisesta standard erroriin C#:ssä](https://docs.microsoft.com/fi-fi/dotnet/api/system.console.error)
- [Ohjeita virheilmoitusten hallintaan ohjelmoinnissa](https://www.toptal.com/software/error-handling-in-programming)