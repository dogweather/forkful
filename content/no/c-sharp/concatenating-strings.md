---
title:    "C#: Sammenkjeding av strenger"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

##Hvorfor
Noen ganger, i vår C# programmering, trenger vi å kombinere to strenger for å lage en ny og lengre streng. Dette kalles å "konkatenere" strenger og kan være nyttig for å lage dynamiske meldinger og tekstutskrifter.

##Slik gjør du det
For å konkatenerere strenger i C#, kan du bruke "+" operatøren. For eksempel:

```C#
string navn = "Per";
string etternavn = "Andersen";

string fulltNavn = navn + " " + etternavn;

Console.WriteLine(fulltNavn);
```

Dette vil skrive ut "Per Andersen" i konsollen.

Du kan også bruke String.Format metoden for å konkatenerere strenger på en mer strukturert måte:

```C#
string navn = "Per";
string etternavn = "Andersen";

string fulltNavn = String.Format("{0} {1}", navn, etternavn);

Console.WriteLine(fulltNavn);
```

Dette vil også skrive ut "Per Andersen" i konsollen.

##Dypdykk
Når du konkatenere strenger i C#, må du være oppmerksom på at resultatet blir en ny streng hver gang. Dette betyr at hvis du gjør dette i en løkke eller i en større applikasjon, kan det føre til unødvendig bruk av minne og ressurser.

En annen ting å huske på er at du ikke kan konkatenere strenger med forskjellige datatyper. For eksempel kan du ikke legge til et tall og en streng sammen.

##Se også
- [Microsoft sin dokumentasjon om String.Concat metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=netframework-4.8)
- [C# string handling](https://www.csharp-station.com/Tutorial/CSharp/lesson14)
- [Konkatenere strenger i C#](https://www.c-sharpcorner.com/article/concatenate-strings-in-C-Sharp/)