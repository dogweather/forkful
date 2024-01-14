---
title:    "C#: Muuntamassa merkkijonoa pieniksi kirjaimiksi"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi: Merkkijonon muuntaminen pieniksi kirjaimiksi

Merkkijonon muuntaminen pieniksi kirjaimiksi on hyödyllinen taito, jota tarvitaan usein ohjelmoinnissa. Pienet kirjaimet voivat olla tärkeitä esimerkiksi merkkijonojen vertailussa tai tietokannan hakutoiminnoissa. Se voi myös tehdä koodista helpommin luettavaa ja yhtenäistä.

## Kuinka tehdä se: Koodiesimerkkejä ja tulosteita

```C#
string s = "MOllaMOlla";
string lowerCase = s.ToLower();
Console.WriteLine(lowerCase);
// Output: mollamolla
```

Tässä esimerkissä käytetään C# -kielen sisäistä ToLower() -metodia. Se luo uuden merkkijonon, jossa kaikki alkuperäisen merkkijonon kirjaimet ovat pieniä. Tämä toimii myös kirjainmerkkijonojen ja luvuista muodostuvien merkkijonojen kanssa.

```C#
string s = "HUUTO!!";
string lowerCase = s.ToLower();
Console.WriteLine(lowerCase);
// Output: huuto!!
```

Toinen vaihtoehtoinen tapa on käyttää LINQ -metodia Select(). Se tarvitsee pääsyä System.Linq -kirjastoon. Tämä metodi antaa mahdollisuuden käsitellä jokaista merkkiä erikseen. Tässä esimerkissä käytämme Char.ToLower() -metodia, joka määrittää merkin alakirjaimeksi.

```C#
string s = "TÄÄ ON KIRJOITETTU ISOILLA KIRJAIMILLA";
string lowerCase = new string(s.Select(c => Char.ToLower(c)).ToArray());
Console.WriteLine(lowerCase);
// Output: tää on kirjoitettu isoilla kirjaimilla
```

## Syvällisempi sukellus

Merkkijonon muuttaminen pieniksi kirjaimiksi on käsitteenä melko yksinkertainen, mutta siihen liittyy muutamia tärkeitä asioita, jotka on hyvä pitää mielessä:

- Pienet ja isot kirjaimet eivät aina vastaa toisiaan. Esimerkiksi ä ja a eivät ole aina samoja merkkejä, vaikka ne voivat näyttää samalta.
- Joissain kielissä (kuten suomi) on myös lisämerkkejä, jotka eivät ole pieniä tai isoja. Nämä pitää käsitellä erikseen.
- Jos haluat muuntaa merkkijonon takaisin isoiksi kirjaimiksi, voit käyttää metodia ToUpper() samalla tavalla kuin ToLower(). 

## Katso myös

- [Documentation for ToLower() method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Documentation for Select() method in LINQ](https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.select)