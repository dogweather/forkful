---
title:                "Päivämäärän jäsennys merkkijonosta"
aliases:
- /fi/c-sharp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:53.948286-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsennys merkkijonosta C#:ssa tarkoittaa päivämäärien ja aikojen tekstiesitysten muuntamista `DateTime`-objektiksi. Tämä on välttämätöntä sovelluksille, jotka tarvitsevat manipuloida, tallentaa tai näyttää päivämääriä ja aikoja eri muodoissa, kuten aikataulusovellukset, lokiprosessorit tai mikä tahansa järjestelmä, joka käsittelee päivämääräsyötettä käyttäjiltä tai ulkoisista lähteistä.

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
