---
title:                "C#: Sammenligne to datoer"
simple_title:         "Sammenligne to datoer"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor?

Har du noen gang lurt på hvordan man kan sammenligne to datoer i et programmeringsspråk som C#? Vel, denne bloggposten er akkurat det du trenger. Enten du er nybegynner eller erfaren utvikler, vil du sikkert dra nytte av å lære hvordan man sammenligner datoer i C#. La oss dykke rett inn i det!

# Hvordan gjøre det?

Å sammenligne to datoer i C# kan gjøres ved hjelp av `DateTime`-strukturen. Her er et eksempel på hvordan man kan sammenligne en dato med dagens dato:

```C#
DateTime today = DateTime.Today;
DateTime otherDate = new DateTime(2020, 10, 15);

if (today > otherDate)
{
    Console.WriteLine("Today is after October 15th, 2020.");
}
else if (today < otherDate)
{
    Console.WriteLine("Today is before October 15th, 2020.");
}
else
{
    Console.WriteLine("Today is October 15th, 2020.");
}
```

I dette eksempelet bruker vi sammenligningsoperatørene `>` og `<` for å sammenligne datoer. Vi kan også bruke `==` operatøren for å se om datoene er like.

```
Output: Today is after October 15th, 2020.
```

# Dykk dypere

Når man sammenligner datoer, kan det være lurt å være oppmerksom på at tidspunktet også blir tatt med i betraktningen. For eksempel, hvis vi endrer `today` til å inkludere klokkeslettet, vil resultatet av sammenligningen bli annerledes. Se på dette eksempelet:

```C#
DateTime today = new DateTime(2020, 10, 15, 12, 0, 0);
DateTime otherDate = new DateTime(2020, 10, 15, 0, 0, 0);

if (today > otherDate)
{
    Console.WriteLine("Today is after October 15th, 2020.");
}
else if (today < otherDate)
{
    Console.WriteLine("Today is before October 15th, 2020.");
}
else
{
    Console.WriteLine("Today is October 15th, 2020.");
}
```

```
Output: Today is after October 15th, 2020.
```

Som du kan se, endret bare klokkeslettet på `today` datoens posisjon i forhold til `otherDate`.

# Se også

- [C# DateTime Struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [DateTime Comparison in C#](https://www.c-sharpcorner.com/blogs/date-comparison-in-c-sharp1)