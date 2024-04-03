---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:53.948286-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta C#:ssa tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4rien ja aikojen tekstiesitysten muuntamista `DateTime`-objektiksi.\
  \ T\xE4m\xE4 on v\xE4ltt\xE4m\xE4t\xF6nt\xE4\u2026"
lastmod: '2024-03-13T22:44:56.582679-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta C#:ssa tarkoittaa p\xE4\
  iv\xE4m\xE4\xE4rien ja aikojen tekstiesitysten muuntamista `DateTime`-objektiksi."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Kuinka:
**Perusjäsennys:**

`DateTime.Parse` ja `DateTime.TryParse` -menetelmät ovat go-to-vaihtoehtoja muuntaaksesi merkkijonon `DateTime`-objektiksi. Tässä on nopea esimerkki:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Onnistuneesti jäsennetty: {parsedDate}");
}
else
{
    Console.WriteLine("Jäsennys epäonnistui.");
}
// Tuloste: Onnistuneesti jäsennetty: 12.4.2023 0:00:00
```

**Kulttuurin määrittäminen:**

Joskus sinun on jäsenettävä päivämäärämerkkijono, joka on tietyn kulttuurin muodossa. Tämän voit saavuttaa käyttämällä `CultureInfo`-luokkaa:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Tuloste: 12.4.2023 0:00:00
```

**Tarkka jäsennys tietyllä muodolla:**

Skenaarioissa, joissa päivämäärät tulevat tietyssä muodossa, joka ei välttämättä ole standardi, `DateTime.ParseExact` tulee tarpeeseen:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Tuloste: 12.4.2023 0:00:00
```

**NodaTime:n käyttö:**

Viela robustimpaan päivämäärän ja ajan jäsennykseen, harkitse suositun kolmannen osapuolen kirjaston NodaTime käyttöä. Se tarjoaa laajemman valikoiman päivämäärän/aika käsittelyn mahdollisuuksia:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Jäsennys epäonnistui.");
}
```

NodaTime tarjoaa laajamittaista tukea aikavyöhykkeille, ajanjaksoille ja kestokonsepteille sekä monille eri kalenterijärjestelmille, tehden siitä voimakkaan valinnan monimutkaisten päivämäärän ja ajan manipulointiin .NET-sovelluksissa.
