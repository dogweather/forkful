---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Streng-sammenføyning er en prosedyre for å koble sammen/enhetliggjøre flere strenger (sentenser, ord etc.) til en enkelt streng. Dette gjør vi for å lage dynamiske strenger, manipulere data eller tilpasse resultater til spesifikke formater.

## Hvordan?
Her er eksempelkode som demonstrerer strengsammenføying i C#:
```csharp
string s1 = "Hallo, ";
string s2 = "Verden!";
string s3 = s1 + s2;
Console.WriteLine(s3);
```
Produksjonen av koden vil være:
```
Hallo, Verden!
```
C# tilbyr også String.Concat()-metoden for å lage strengkombinasjoner:
```csharp
string s1 = "Hallo, ";
string s2 = "Verden!";
string s3 = String.Concat(s1, s2);
Console.WriteLine(s3);
```
Og produksjonen vil være det samme: 
```
Hallo, Verden!
```
## Dyp Dykk
Historien for strengsammenføying går tilbake til de tidlige dager med programmering. Det var alltid behov for å sette sammen flere biter av tekst.

I C# er det mange måter å administrere strengsammenføyning på. Den tradisjonelle operatøren (+) og String.Concat()-metoden er de mest kjente, men det er også andre løsninger, for eksempel String.Join() og StringBuilder.Append().

Når det gjelder ytelse, avhenger det av mengden data som skal behandles. For mindre volumer gir de forskjellige teknikkene omtrent lik ytelse. Men for store volumer kan StringBuilder være mer effektiv siden det ikke lager unødvendige mellomliggende strenger og derfor bruker mindre minne.

## Se også
[Microsofts dokumentasjon om Streng-sammenføyning](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
[String.Join-metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.join?view=net-5.0)
[StringBuilder-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)