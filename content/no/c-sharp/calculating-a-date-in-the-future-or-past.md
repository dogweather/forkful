---
title:    "C#: Beregning av en dato i fremtiden eller fortiden."
keywords: ["C#"]
---

{{< edit_this_page >}}

# Hvorfor 

Å beregne datoer i fremtiden eller fortiden kan være en viktig del av programmeringsarbeid, da dette kan hjelpe oss å planlegge hendelser eller håndtere tidssensitive data. Ved å implementere denne funksjonaliteten i våre programmer kan vi forutsi og planlegge fremtidige hendelser eller håndtere historiske data på en effektiv måte.

# Hvordan

**For å beregne en dato i fremtiden:**

```C#
DateTime today = DateTime.Today; // Dagens dato
DateTime futureDate = today.AddDays(10); // Beregner datoen 10 dager fram i tid
Console.WriteLine("Datoen 10 dager fra i dag er: " + futureDate);
```

**Output:**

Datoen 10 dager fra i dag er: 04/06/2021

**For å beregne en dato i fortiden:**

```C#
DateTime today = DateTime.Today; // Dagens dato
DateTime pastDate = today.AddDays(-7); // Beregner datoen 7 dager tilbake i tid
Console.WriteLine("Datoen 7 dager tilbake i tid er: " + pastDate);
```

**Output:**

Datoen 7 dager tilbake i tid er: 03/24/2021

# Dypdykk

Det er flere metoder i C# som kan hjelpe oss med å beregne datoer i fremtiden eller fortiden, som for eksempel `.AddDays()`, `.AddMonths()`, `.AddYears()`, `.AddHours()`, `.AddMinutes()` og `.AddSeconds()`. Disse metodene tar inn et tall som representerer antall dager, måneder, år, timer, minutter eller sekunder for å beregne den ønskede datoen.

Vi kan også bruke `.Add()` metoden for å legge til både dato og tid. For eksempel:

```C#
DateTime today = DateTime.Today; // Dagens dato kl 00:00:00
DateTime futureDateTime = today.Add(new TimeSpan(4, 6, 30, 0)); // Legger til 4 dager, 6 timer og 30 minutter for å beregne en fremtidig dato og tid
Console.WriteLine("Fremtidig dato og tid: " + futureDateTime);
```

**Output:**

Fremtidig dato og tid: 03/28/2021 06:30:00

For mer avansert håndtering av dato og tid, kan vi også bruke biblioteket `System.Globalization` som gir oss flere muligheter for å formatere og manipulere datoer i forskjellige kulturer og språk.

# Se Også
 
Her er noen ekstra ressurser for å lære mer om å beregne datoer i C#:

- [MSDN: DateTime Struct (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# Tutorial: Working with DateTime in C#](https://www.c-sharpcorner.com/UploadFile/b3727a/working-with-datetime-in-C-Sharp/)
- [C# Tutorial: Date and Time in C#](https://www.guru99.com/c-sharp-date-time.html)
- [C# Tutorial: How to Add or Subtract Days, Months, Years and Time from DateTime in C#](https://www.c-sharpcorner.com/blogs/how-to-add-or-subtract-days-months-years-and-time-from-datetime-in-c-sharp-programming1)