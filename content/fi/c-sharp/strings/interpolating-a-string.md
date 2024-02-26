---
changelog:
- 2024-02-25, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-02-25 17:06:58.048640-07:00
description: "Merkkijonon interpolaatio C#:ssa mahdollistaa uuden merkkijonon luomisen\
  \ sis\xE4llytt\xE4m\xE4ll\xE4 lausekkeita merkkijonoliteraalin sis\xE4\xE4n, mik\xE4\
  \ tekee merkkijonojen\u2026"
lastmod: '2024-02-25T18:49:53.474621-07:00'
model: gpt-4-0125-preview
summary: "Merkkijonon interpolaatio C#:ssa mahdollistaa uuden merkkijonon luomisen\
  \ sis\xE4llytt\xE4m\xE4ll\xE4 lausekkeita merkkijonoliteraalin sis\xE4\xE4n, mik\xE4\
  \ tekee merkkijonojen\u2026"
title: Merkkijonon interpolointi
---

{{< edit_this_page >}}

## Mikä ja miksi?
Merkkijonon interpolaatio C#:ssa mahdollistaa uuden merkkijonon luomisen sisällyttämällä lausekkeita merkkijonoliteraalin sisään, mikä tekee merkkijonojen muotoilusta ja yhdistämisestä helpompaa. Ohjelmoijat käyttävät tätä ominaisuutta parantaakseen koodin luettavuutta ja ylläpidettävyyttä, erityisesti käsiteltäessä dynaamista merkkijonosisältöä.

## Kuinka:
C#:ssa merkkijonon interpolaatio merkitään dollarimerkillä (`$`) seurattuna merkkijonoliteraalista. Muuttujan nimet tai lausekkeet sijoitetaan aaltosulkeisiin (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hei, {name}! Olet {age} vuotta vanha.";
Console.WriteLine(interpolatedString);
// Tuloste: Hei, Jane! Olet 28 vuotta vanha.
```

Monimutkaisemmassa esimerkissä voit suorittaa operaatioita tai kutsua metodeja aaltosulkujen sisällä:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Kokonaishinta: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Tuloste: Kokonaishinta: 59,97 $
```
Merkkijonon sisällä aaltosulkujen sisäinen `:C2` muotoilumääre muotoilee numeron valuuttana kahdella desimaalipaikalla.

Skenaarioissa, jotka vaativat monimutkaisempaa muotoilua tai lokalisointia, kannattaa harkita `string.Format` -metodin käyttöä tai kirjastoja, kuten Humanizer. Humanizer voi manipuloida ja näyttää merkkijonoja, päivämääriä, aikoja, aikavälejä, numeroita ja määriä ihmisen luettavammassa muodossa. Alla on esimerkki Humanizerin käytöstä monimutkaisessa merkkijonomanipulaatiossa. Huomaa, että Humanizer ei ole osa .NET-standardikirjastoa ja sen asentaminen vaatii NuGet-paketin `Humanizer`.

Asenna Humanizer ensin NuGetin kautta:

```
Install-Package Humanizer
```

Tämän jälkeen voit käyttää sitä seuraavasti:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"Tapahtuma oli {dayDifference} päivää sitten.".Humanize();
Console.WriteLine(humanized);
// Riippuen konfiguraatiosta ja kulttuurista, mahdollinen tuloste: Tapahtuma oli 5 päivää sitten.
```

Tämä esimerkki osoittaa peruskäytön. Humanizer tukee laajaa valikoimaa toiminnallisuuksia, joita voidaan soveltaa merkkijonoihin, päivämääriin, numeroihin ja muuhun, tehden sovelluksistasi helpommin saavutettavia ja intuitiivisia.
