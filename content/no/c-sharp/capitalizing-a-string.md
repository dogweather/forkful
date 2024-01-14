---
title:    "C#: Kapitalisering av en streng"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

I mange programmeringsspråk, som for eksempel C#, er tekststrenger en viktig del av koden. Disse tekststrengene kan ofte bli presentert for brukeren i form av et grensesnitt eller utskrifter i konsollen. Derfor er det viktig at disse strengene blir formatert på en forståelig og leselig måte. En vanlig formatering er gjennom å "kapitalisere" teksten, altså å gjøre den store bokstaver.

## Hvordan

For å kapitalisere en tekststreng i C#, kan vi bruke innebygde funksjoner som “ToUpper()” og “FirstCharToUpper()”. La oss se på et eksempel:

```C#
string tekst = "dette er en tekst";
Console.WriteLine(tekst.ToUpper()); // Utskrift: DETTE ER EN TEKST
Console.WriteLine(CultureInfo.InvariantCulture.TextInfo.ToTitleCase(tekst)); // Utskrift: Dette Er En Tekst
```

I dette eksempelet bruker vi funksjonen "ToUpper()" for å gjøre hele teksten om til store bokstaver, og deretter bruker vi "ToTitleCase()" for å gjøre første bokstav i hvert ord stor.

Vi kan også bruke "ToLower()" for å gjøre teksten om til små bokstaver, eller "FirstCharToLower()" for å gjøre første bokstav i teksten liten.

## Dypdykk

I C# finnes det flere måter å kapitalisere en tekststreng på. Vi kan for eksempel bruke "Char.ToUpper()" for å gjøre en enkelt bokstav stor, eller "TextInfo.ToTitleCase()" for å formatere en tekst med forskjellige ord.

Det er også viktig å merke seg at kapitalisering kan variere avhengig av språket som brukes. I C# vil for eksempel kapitaliseringen av "i" være annerledes i norsk enn i engelsk.

## Se også

- [ToLower(), ToUpper() og ToTitleCase() - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)
- [Annen formatering av tekst - CSharpdocs](https://csharpdocs.com/c-sharp-text-formatting-guide.html)
- [Eksempler på kapitalisering av tekst - Programiz](https://www.programiz.com/csharp-programming/examples/capitalize-string)