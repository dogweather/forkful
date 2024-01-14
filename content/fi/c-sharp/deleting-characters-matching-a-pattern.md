---
title:                "C#: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### Miksi
Monet koodaajat löytävät itsensä joutumassa tilanteeseen, jossa heidän täytyy poistaa merkkejä tietystä kaavasta ohjelmoidessaan. Tämä voi johtua esimerkiksi virheellisistä syötteistä tai tarpeesta muokata tietoja ennen kuin ne tallennetaan tietokantaan. Aloittelevat koodaajat saattavat ihmetellä, miksi ja miten tällaista toimintoa tulisi käyttää. Tässä blogikirjoituksessa käymme läpi, miten voit poistaa merkkejä määritetyn kaavan mukaisesti C# -ohjelmointikielessä.

### Miten
Seuraavassa on kaksi esimerkkiä, jotka auttavat ymmärtämään, miten voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa C# -ohjelmointikielessä. Ensimmäinen esimerkki käyttää String.Replace -metodia ja toinen esimerkki käyttää Regex.Replace -metodia.

```C#
// Esimerkki 1: Käyttämällä String.Replace -metodia
string text = "Tämä on esimerkki1234";
string modifiedText = text.Replace("1234", "");
Console.WriteLine(modifiedText);
// Output: Tämä on esimerkki

// Esimerkki 2: Käyttämällä Regex.Replace -metodia
string text = "Tämä on esimerkki1234";
string modifiedText = Regex.Replace(text, @"\d", "");
Console.WriteLine(modifiedText);
// Output: Tämä on esimerkki
```

Näissä esimerkeissä merkkijono "1234" korvataan tyhjällä merkkijonolla. Voit vaihtaa tämän haluamaasi kaavaan, jolloin se poistaa kaikki merkit, jotka vastaavat kaavaa.

### Syvempi sukellus
C# -ohjelmointikielestä löytyy useita erilaisia metodeja, joita voit käyttää poistaaksesi merkkejä, jotka vastaavat tiettyä kaavaa. Yllä olevien esimerkkien lisäksi voit esimerkiksi käyttää StringBuilder -luokan Replace metodia tai luoda oman metodin, joka tarkistaa merkkijonon jokaisen merkin ja poistaa sen, jos se vastaa tarvittavaa kaavaa.
On myös tärkeää muistaa, että jokainen metodi toimii hieman eri tavalla ja joissain tapauksissa tietty metodi voi olla parempi vaihtoehto kuin toinen, joten tutustu erilaisiin vaihtoehtoihin ja valitse itsellesi sopivin tapa.

### Katso myös
- [String.Replace Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [Regex.Replace Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
- [C# -ohjelmointikielen käyttöönotto (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/csharp/getting-started/)