---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän jäsentäminen merkkijonosta tarkoittaa kirjaimellisesti merkkijonoa, joka on kirjoitettu päivämäärämuodossa, ja sen muuttamista päivämäärä- tai aikaolennoksi. Ohjelmoijien on tehtävä tämä, kun heidän on käsiteltävä päivämääriä, jotka on alun perin kirjoitettu tekstiin.

## Näin teet:

Voimme jäsentää päivämäärän merkkijonosta C# -ohjelmointikielen `DateTime.Parse` ja `DateTime.TryParse` -metodeilla. Katso alla oleva esimerkki.

```C#
using System;

public class Ohjelma
{
    public static void Main()
    {
        string pvmMerkkijonona = "15.09.2022";
        DateTime yritettyPvm = DateTime.Parse(pvmMerkkijonona);
        Console.WriteLine(yritettyPvm);
    }
}
```

Tulostus:

```
2022-09-15 00:00:00
```

## Sukellus syvemmälle:

Päivämäärän jäsentämiseen merkkijonosta on olemassa useita menetelmiä, mutta C# on valinnut `DateTime.Parse` ja sen turvallisemman siskon `DateTime.TryParse`. Historiallisesti tässä on ollut useita kompastuskiviä, kuten alueelliset päivämääräformaattierot ja päivämääräformaattien vakiona pidettäminen.

Yksi vaihtoehtoinen tapa on `DateTime.ParseExact`, joka ottaa huomioon tietyn päivämääräformaatin. Tämä tarjoaa tarkemman hallinnan merkkijonon muuntamisesta päivämääräksi.

```C#
using System;
using System.Globalization;

public class Ohjelma
{
    public static void Main()
    {
        string pvmMerkkijonona = "15 September 2022";
        string formaatti = "dd MMMM yyyy";
        DateTime yritettyPvm = DateTime.ParseExact(pvmMerkkijonona, formaatti, CultureInfo.InvariantCulture);
        Console.WriteLine(yritettyPvm);
    }
}
```

Tulostus:

```
2022-09-15 00:00:00
```

## Katso myös:

Ohjelmoijille voisi olla hyötyä näistä aiheeseen liittyvistä lähteistä:

- [DateTime.Parse Method (Microsoft docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-5.0)
- [DateTime.TryParse Method (Microsoft docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse?view=net-5.0)
- [DateTime.ParseExact Method (Microsoft docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-5.0)