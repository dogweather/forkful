---
title:                "Tekstitiedoston lukeminen"
html_title:           "C#: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Tekstitiedoston lukeminen tarkoittaa tiedon hakemista tiedostosta ja sen avaamista ohjelman käyttöön. Tämä on tärkeä taito ohjelmoinnissa, sillä usein ohjelmat tarvitsevat tietoa ulkoisista lähteistä, kuten käyttäjän syöttämistä tiedoista tai tallennetuista tiedostoista.

# Miten:
Käytännön esimerkki: 
```C#
string tiedostonSisalto = System.IO.File.ReadAllText("tiedosto.txt");
Console.WriteLine(tiedostonSisalto);
```
Tämä koodinpätkä lukee tiedoston nimeltä "tiedosto.txt" ja tallentaa sen sisällön muuttujaan nimeltä "tiedostonSisalto". Tämän jälkeen sisältö tulostetaan näytölle.

# Syventävä tarkastelu:
Tekstitiedoston lukeminen on ollut ohjelmoinnin perustaito jo pitkään. Aikaisemmin se saattoi vaatia enemmän koodin kirjoittamista, mutta nykyään C# tarjoaa tähän yksinkertaisen ja helpon ratkaisun. Tekstitiedoston lukemisen lisäksi voidaan käyttää myös muita tapoja, kuten tiedoston avaamista ja lukuun tarkoitettuja luokkia.

# Katso myös:
- https://www.c-sharpcorner.com/article/reading-and-writing-text-file-in-c-sharp/
- https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext