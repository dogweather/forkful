---
title:    "C#: Å finne lengden på en streng"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Visste du at å finne lengden på en streng er en vanlig oppgave i mange programmeringsspråk? Å finne lengden på en streng kan gi nyttig informasjon som kan bidra til å effektivisere koden din. I denne bloggposten vil vi se nærmere på hvorfor det er viktig å kunne finne lengden på en streng, og hvordan du kan gjøre det i C#. 

## Hvordan gjøre det 

For å finne lengden på en streng i C#, kan vi bruke `Length`-metoden. Her er et enkelt eksempel: 

```C#
string navn = "Marie";
int lengde = navn.Length;
Console.WriteLine(lengde); // output: 5
```

I dette tilfellet vil 5 bli skrevet ut, siden lengden på strengen "Marie" er fem tegn. Vi kan også bruke `Length`-metoden på variabler som er av typen `StringBuilder`.

```C#
StringBuilder tekst = new StringBuilder("Dette er en tekst");
int lengde = tekst.Length;
Console.WriteLine(lengde); // output: 18
```

Som du ser, er det enkelt å finne lengden på en streng i C#. Dette kan være nyttig når du for eksempel skal håndtere brukerinndata eller behandle tekstbaserte datafiler. 

## Dypdykk

Nå som vi har sett på hvordan vi kan finne lengden på en streng i C#, la oss ta et dypere dykk og se på hva som faktisk skjer når vi bruker `Length`-metoden. 

Denne metoden er en del av standardbiblioteket i C#, og den tilhører typen `String` og `StringBuilder`. Når du bruker `Length`-metoden, returneres antallet tegn i strengen du har gitt den. Dette er nyttig for å finne ut hvor mange tegn som må behandles når du for eksempel arbeider med tekstbaserte datafiler. Husk at mellomrom og spesialtegn også telles med når du finner lengden på en streng. 

I tillegg til å bruke `Length`-metoden, kan du også bruke `Count`-egenskapen. Forskjellen mellom disse to er at `Length`-metoden er en metode som returnerer et heltall, mens `Count` er en egenskap som returnerer en long-verdi. Begge gir likevel samme resultat når du bruker dem til å finne lengden på en streng. 

## Se også

Her er noen nyttige lenker til ressurser som kan hjelpe deg å lære mer om lengden til en streng i C#:

* [Microsoft Docs om string.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1)
* [W3Schools C# Strings tutorial](https://www.w3schools.com/cs/cs_strings.asp)
* [GeeksforGeeks om string.Length Property](https://www.geeksforgeeks.org/c-sharp-string-length-property/)