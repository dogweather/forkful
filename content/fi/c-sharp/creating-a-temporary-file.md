---
title:                "Luodaan väliaikainen tiedosto"
html_title:           "C#: Luodaan väliaikainen tiedosto"
simple_title:         "Luodaan väliaikainen tiedosto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi
Temporary filejen luominen on hyödyllistä tilanteissa, joissa halutaan väliaikaisesti tallentaa tai käsitellä tietoja ohjelmassa. Tämä voi auttaa välttämään pysyvien muutosten tekemistä, mikäli tiedot eivät ole lopullisia tai tarpeeksi tarkkoja.

## Kuinka
Voidaksesi luoda temporary filen C#-ohjelmassa, käytä "System.IO.Path.GetTempFileName()" -metodia ja anna sille haluttu nimi parametrinä. Tämä metodi luo tiedoston väliaikaisessa hakemistossa ja palauttaa sen polun kutsujalle.

```C#
string tempFilePath = System.IO.Path.GetTempFileName("tilapainen.txt");
Console.WriteLine("Temporary file created at: " + tempFilePath);

// Output:
// Temporary file created at: C:\Users\<username>\AppData\Local\Temp\tilapainen.txt
```

Oletuksena temporary filet poistetaan automaattisesti, kun ne eivät enää ole käytössä. Mikäli haluat poistaa tiedoston manuaalisesti, voit käyttää "System.IO.File.Delete()" -metodia ja antaa sille temporary filen polun parametrinä.

## Deep Dive
Temporary filejen luominen on usein tarpeellista silloin, kun ohjelma käsittelee suurta määrää dataa ja tarvitsee väliaikaisen tallennuspaikan. Tämä voi esimerkiksi olla hyödyllistä tietokonelaskennassa, jossa datan käsittelyyn tarvitaan enemmän muistia kuin mitä koneella on saatavilla. Temporary file voi myös toimia turvallisempana vaihtoehtona kuin käyttöjärjestelmän välimuisti, sillä se varmistaa, että tiedosto poistetaan käytön jälkeen.

Temporary filen polkua voidaan myös käyttää viestinvälitykseen eri komponenttien välillä. Esimerkiksi jos ohjelmassa on useampia säikeitä, voidaan temporary filea käyttää kommunikointiin niiden välillä.

## Katso myös
- [C#-oppaat ja dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/csharp/)
- [Temporary files MSDN-dokumentaatiossa](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-temporary-files)
- [Temporary filejen turvallisuus](https://www.schneier.com/blog/archives/2010/06/creating_tempor.html)