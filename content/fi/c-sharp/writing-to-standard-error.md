---
title:                "C#: Kirjoittaminen standardivirheeseen"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit kirjoittaa standard error -tulostusvirtaan?

Standard error on tärkeä osa ohjelmoinnin virheenkorjausta. Se mahdollistaa ohjelmalle antamasi ilmoitukset ja virheilmoitukset näkyvän selkeästi terminaalissa tai muussa komentokehotteessa. Tämä auttaa sinua paikantamaan ja korjaamaan ohjelman mahdolliset virheet nopeasti ja tehokkaasti.

## Miten

### Esimerkki 1: Yksinkertainen virheilmoitus

Seuraava koodiesimerkki näyttää yksinkertaisen tavan kirjoittaa virheilmoitus standard error -tulostusvirtaan:

```C#
Console.Error.WriteLine("Ohjelma törmäsi virheeseen!");
```

Tämän koodin suorituksen jälkeen näet virheilmoituksen komentokehotteessa.

```
Ohjelma törmäsi virheeseen!
```

### Esimerkki 2: Virheilmoituksen lisääminen poikkeuskäsittelyyn

Useimmissa tapauksissa haluat kirjoittaa virheilmoituksen standard error -tulostusvirtaan, kun ohjelmassa tapahtuu virhe. Tämä voidaan tehdä poikkeuskäsittelyllä seuraavasti:

```C#
try
{
  // Suorita jotain ohjelmaa
}
catch (Exception ex)
{
  Console.Error.WriteLine("Virhe: " + ex.Message);
}
```

Tässä esimerkissä, jos ohjelma törmää virheeseen, näet virheilmoituksen, joka sisältää kyseisen virheen viestin.

```
Virhe: Ohjelma kohtasi odottamattoman virheen.
```

## Syventävä sukellus

### Erot standard outputin ja standard errorin välillä

On tärkeää ymmärtää ero standard outputin (Console.WriteLine) ja standard errorin (Console.Error.WriteLine) välillä. Standard output käytetään normaaliin ohjelman tulostukseen, kun taas standard erroria käytetään virheilmoituksissa ja poikkeuskäsittelyssä.

Jos ohjelman suorittamisessa ei tapahdu virhettä, standard output ja standard error molemmat ohjautuvat yleensä samaan paikkaan, esimerkiksi terminaaliin tai komentokehotteeseen. Jos ohjelma kuitenkin kohtaa virheen, standard outputin tulostama tieto saattaa hävitä tai sekoittua virheilmoitusten joukkoon. Tästä syystä on tärkeää käyttää standard erroria virheilmoituksissa, jotta ohjelman suorituksen tulos olisi selkeä ja helppo tarkistaa.

## Katso myös

- [C# Console.Error Property](https://docs.microsoft.com/en-us/dotnet/api/system.console.error?view=net-5.0)
- [C# Exception Class](https://docs.microsoft.com/en-us/dotnet/api/system.exception?view=net-5.0)
- [Debugging in C#: Basic Techniques](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-in-csharp-basic-techniques?view=vs-2019)