---
title:    "C#: Kirjoittaminen standardivirheelle"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

On monia erilaisia syitä, miksi haluat kirjoittaa standardeihin virheisiin ohjelmoinnissa. Yleisimpiä syitä ovat virheiden havaitseminen ja korjaaminen sekä ongelmanratkaisu ohjelmoinnissa.

## Miten

Käyttämällä C# -kieltä, voit helposti kirjoittaa esimerkkilausekkeita ja -tulosteita käyttämällä "```C# ... ```" -koodilohkoja.

```C#
Console.WriteLine("Tämä on virheilmoitus!"); // Tulostaa tekstin "Tämä on virheilmoitus!" standardeihin virheisiin.
```

Kun tämä koodi suoritetaan, se tulostaa tekstin "Tämä on virheilmoitus!" standardeihin virheisiin, jolloin voit havaita ja korjata mahdollisia virheitä koodissasi.

## Syvällinen sukellus

Standardeihin virheisiin kirjoittaminen on tärkeä osa ohjelmoinnin ongelmanratkaisuprosessia. Se mahdollistaa virheiden havaitsemisen ja korjaamisen, mikä johtaa parempaan ohjelmiston laatuun. Lisäksi standardin virheilmoitukset auttavat myös muiden kehittäjien ymmärtämään ja työskentelemään koodisi kanssa.

## Katso myös

- [C#:n kirjastoluokka Console](https://docs.microsoft.com/fi-fi/dotnet/api/system.console?view=net-5.0)
- [Virheiden hallinta C#:ssa](https://docs.microsoft.com/fi-fi/dotnet/csharp/fundamentals/exceptions/)
- [Ohjelmoinnin ongelmanratkaisun vinkkejä](https://www.hackerrank.com/domains/tutorials/30-days-of-code)