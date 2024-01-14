---
title:    "C#: Utvinning av delstrenger"
keywords: ["C#"]
---

{{< edit_this_page >}}

#

## Hvorfor

Å utvinne substrings er en vanlig oppgave i programmering, spesielt innenfor tekstbehandling og behandling av strenger. Det kan hjelpe deg med å manipulere og bearbeide data på en enklere og mer effektiv måte.

## Hvordan

For å utvinne substrings i C # kan du bruke metoden `Substring()`. Denne metoden tar to parametere: startindeksen og lengden på den ønskede substringen. La oss se på et eksempel:

```C#
string tekst = "Dette er en tekststreng som vi vil utvinne substrings fra";

string substring1 = tekst.Substring(5, 10);
Console.WriteLine(substring1); // Output: er en tekst

string substring2 = tekst.Substring(22, 6);
Console.WriteLine(substring2); // Output: vil ut
```

Her har vi brukt `Substring()` -metoden for å utvinne en del av `tekst` -strengen, som starter fra indeks 5 og har en lengde på 10 tegn. Vi har også utvunnet en annen del av strengen som starter fra indeks 22 og har en lengde på 6 tegn.

Du kan også bruke `Substring()` -metoden på en mer dynamisk måte ved å bruke variabler i stedet for tall. For eksempel:

```C#
int start = 5;
int lengde = 10;

string tekst = "Dette er en tekststreng som vi vil utvinne substrings fra";

string substring = tekst.Substring(start, lengde);
Console.WriteLine(substring); // Output: er en tekst
```

Dette kan være nyttig hvis du vil lage en funksjon som utviner substrings av forskjellige deler av en streng, basert på brukerens inndata.

## Dypdykk

`Substring()` -metoden har også en overbelastet versjon som tar bare én parameter, som er startindeksen. Hvis dette er tilfelle, vil metoden utvinne en substring som starter fra den gitte indeksen og frem til slutten av strengen.

```C#
string tekst = "Dette er en tekststreng som vi vil utvinne substrings fra";

string substring = tekst.Substring(9);
Console.WriteLine(substring); // Output: en tekststreng som vi vil utvinne substrings fra
```

Det er også verdt å nevne at `Substring()` -metoden ikke endrer den opprinnelige strengen, den returnerer bare den utvinne substringen. Hvis du vil endre den opprinnelige strengen, må du lagre den returnerte substringen i en ny variabel eller tilbakestille den opprinnelige strengen.

## Se også

* [Dokumentasjon: String.Substring metode (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
* [C # Strenger Tutorial (W3Schools)](https://www.w3schools.com/cs/cs_strings.asp)