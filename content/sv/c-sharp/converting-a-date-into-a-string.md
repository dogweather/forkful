---
title:                "Konvertera ett datum till en sträng"
html_title:           "C#: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & varför?

Konvertering av datum till en sträng är en vanlig uppgift för programmerare. Det innebär att man tar ett datum, som ofta finns i ett speciellt format i datorns minne, och omvandlar det till en läsbar textsträng. Detta kan vara användbart när man vill visa datumet på ett visst sätt för användaren eller när man behöver spara datumet i en textfil eller databas.

## Hur gör man:

### Exempel 1:
```C#
DateTime date = new DateTime(2021, 8, 25);
string dateAsString = date.ToString("dd/MM/yyyy");
Console.WriteLine(dateAsString);
```
Output: 25/08/2021

I detta exempel skapas en ny DateTime-variabel med datumet 25 augusti 2021. Sedan används metoden "ToString" med formatargumentet "dd/MM/yyyy" för att konvertera det till en sträng. Detta resulterar i en sträng som kan användas för att visa eller spara datumet på önskat sätt.

### Exempel 2:
```C#
DateTime date = new DateTime(2021, 8, 25);
string dateAsString = date.ToShortDateString();
Console.WriteLine(dateAsString);
```
Output: 8/25/2021

I detta exempel används metoden "ToShortDateString" istället för att specificera ett format. Detta resulterar i en sträng med datumet i det förinställda formatet för det lokala systemet.

## Djupdykning:

Konvertering av datum till en sträng har utvecklats över tid tillsammans med datorernas kapacitet att hantera datum och tid. Det finns många olika format och metoder för konvertering, beroende på programmeringsspråk och användningsområde.

Förutom metoden "ToString" finns det också andra sätt att konvertera datum till en sträng, som "ToShortDateString" och "ToLongDateString" som visar datumet i olika format. Det finns också möjlighet att anpassa formatet på datumet med hjälp av "Custom DateTime Format Strings" där man kan välja vilka delar av datumet som ska visas och på vilket sätt.

Implementeringen av konvertering av datum till en sträng kan variera beroende på språk och plattform, men grundidén är densamma; att ta ett datum och omvandla det till en textsträng.

## Se även:

- [DateTime.ToString Method (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)