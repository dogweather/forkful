---
title:    "Elixir: Att få den aktuella datumen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

### Varför
Att få den aktuella datumen är en av de grundläggande funktionerna inom programmering. Oavsett om du bygger en kalenderapplikation eller ett enkelt påminnelsesystem, behöver du kunna hämta den nuvarande datumen för att skapa korrekta tidsstämplar. I Elixir finns det flera sätt att få tag på den nuvarande datumen och i den här bloggposten kommer vi att utforska dem.

### Hur du gör
Det enklaste sättet att få den aktuella datumen i Elixir är att använda `Date.utc_today/0`-funktionen. Det här ger ett `Date`-objekt som motsvarar dagens datum enligt UTC-tidszonen.

```Elixir
Date.utc_today()
```

Om du vill ha den nuvarande timmen, minuten och sekunden är `Time.utc_now/0`-funktionen det rätta valet. Det här ger ett `Time`-objekt som representerar nuvarande tiden enligt UTC-tidszonen.

```Elixir
Time.utc_now()
```

För att få datumet och tiden i den lokala tidszonen kan du använda funktionerna `Date.now/0` och `Time.now/0`. Dessa funktioner använder systemets tidszon för att ge den nuvarande datumen och tiden.

```Elixir
Date.now()
Time.now()
```

Det finns också mer specifika funktioner för att få enskilda delar av datum och tid. Till exempel, `DateTime.utc_now/0` ger både datum och tid i UTC-tidszonen.

```Elixir
DateTime.utc_now()
```

Det är också möjligt att ange en specifik tidszon för de datum- och tidstillfällen som du vill ha. Detta är särskilt användbart om du bygger en applikation som måste hantera flera tidszoner. I sådana fall kan du använda `DateTime.from_naive/2`-funktionen och ange tidszonen som andra argumentet.

```Elixir
DateTime.from_naive(DateTime.utc_now(), "Europe/Stockholm")
```

### Djupdykning
Beroende på din applikation kan det vara nödvändigt att göra beräkningar baserat på den nuvarande datumen eller jämföra datum och tider med varandra. I Elixir finns det ett rikt utbud av funktioner för att manipulera datum och tid för att passa dina behov.

För att jämföra två datum kan vi använda `Date.compare/2`-funktionen som returnerar en av tre värden: -1 om det första datumet är tidigare, 0 om de är lika, eller 1 om det andra datumet är senare.

```Elixir
Date.compare(Date.utc_today(), Date.utc_today())
```

Om du vill lägga till eller dra bort en viss tidsperiod, t.ex. en dag eller en timme, kan du använda `Date.add/3` och `Date.diff/3`-funktionerna.

```Elixir
Date.add(Date.utc_today(), 1, :week)
Date.diff(Date.utc_today(), Date.utc_today(), :days)
```

Det finns också funktioner som `Date.day_of_week/1` för att få veckodagen och `Date.day_interval/2` för att få antalet dagar mellan två datum.

### Se också
- [Elixir DateTime-modulen](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Date-modulen](https://hexdocs.pm/elixir/Date.html)
- [Elixir Time-modulen](https://hexdocs.pm/elixir/Time.html)

Tack för att du läste! Vi hoppas att den här bloggposten har gett dig en bättre förståelse för hur du kan få den aktuella datumen i Elixir och hur du kan manipulera datum och tid för dina behov.