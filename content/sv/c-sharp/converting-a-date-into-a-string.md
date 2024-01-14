---
title:    "C#: Omvandla ett datum till en sträng"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Varför

Att konvertera ett datum till en sträng är en vanlig uppgift inom programmering, särskilt när man arbetar med datasystem som måste visa datum och tid på ett läsbart sätt för användare. Det finns olika metoder att utföra detta i C#, men det är viktigt att förstå hur och varför man ska använda dem.

# How To

För att konvertera ett datum till en sträng i C# finns det flera olika inbyggda funktioner att använda sig av. Här är två olika exempel på hur man kan göra detta:

## Exempel 1: Använda ToString() funktionen
```C#
DateTime datum = new DateTime(2021, 10, 23);
string konverteratDatum = datum.ToString("dd/MM/yyyy");
Console.WriteLine(konverteratDatum);

// Output: 23/10/2021
```

## Exempel 2: Använda Format() funktionen
```C#
DateTime datum = new DateTime(2021, 10, 23);
string konverteratDatum = string.Format("{0:dd/MM/yyyy}", datum);
Console.WriteLine(konverteratDatum);

// Output: 23/10/2021
```

I båda dessa exempel använder vi oss av {0:format} för att ange hur vi vill att datumet ska visas som en sträng. I detta fall använder vi formatet "dd/MM/yyyy" för att visa dag, månad, och år i ett specifikt format.

Det finns också andra inbyggda funktioner som kan hjälpa till att konvertera datum till strängar, som till exempel ToString("d") som returnerar datumet i det förinställda kortformatet.

# Deep Dive

Om man vill ha ännu mer kontroll över hur datumet konverteras till en sträng så finns det många olika formateringsalternativ att utforska. För att göra detta måste man förstå betydelsen bakom de symboler som används när man formaterar datumsträngar.

Här är en lista över de vanligaste symbolerna och vad de betyder:

Symbol | Betydelse 
------------- | -------------
dd | Dag i månaden, tvåsiffrig (01-31)
MM | Månad, tvåsiffrig (01-12)
yyyy | År, fyrsiffrig
d | Dag i månaden, en eller tvåsiffrig (1-31)
M | Månad, en eller tvåsiffrig (1-12)
yy | År, tvåsiffrig
HH | Timme, tvåsiffrig (00-23)
mm | Minut, tvåsiffrig (00-59)
ss | Sekund, tvåsiffrig (00-59)

Genom att kombinera dessa symboler på olika sätt kan man skapa olika format för datumsträngen, exempelvis "dd/MM/yy" som ger formatet "23/10/21".

# Se även

För mer information och exempel på hur man konverterar ett datum till en sträng, rekommenderar vi att läsa dokumentationen för DateTime-strukturen i C#. Här är också några relaterade resurser som kan vara av intresse:

- https://docs.microsoft.com/sv-se/dotnet/api/system.datetime.tostring : Dokumentation för ToString() funktionen för DateTime-strukturen.
- https://docs.microsoft.com/sv-se/dotnet/standard/base-types/custom-date-and-time-format-strings : Översikt över de olika symbolerna och hur man kan använda dem för att formatera datumsträngar.
- https://www.c-sharpcorner.com/UploadFile/mkagrahari/date-and-time-format-in-c-sharp : En tutorial som går igenom olika formatteringsalternativ för datum i C#.