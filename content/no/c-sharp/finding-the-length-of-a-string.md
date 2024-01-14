---
title:                "C#: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

For mange programmerere kan det å finne lengden til en streng virke som en enkel oppgave. Men det er faktisk en viktig del av å utvikle effektive og pålitelige applikasjoner. Ved å vite lengden til en streng kan man håndtere tekstbehandling og datastrukturering mer effektivt. Derfor er det viktig å forstå hvordan man kan finne lengden til en streng ved hjelp av C#.

## Slik gjør du det

For å finne lengden til en streng i C#, kan du bruke metoden ".Length". Denne metoden returnerer antall tegn i strengen, inkludert mellomrom og spesialtegn.

```C#
string navn = "Jan";
int lengde = navn.Length;
Console.WriteLine("Lengden til strengen er: " + lengde);
```
Output: Lengden til strengen er: 3

Det er også mulig å hente lengden til en streng som input fra brukeren. Dette kan gjøres ved å bruke metoden "Console.ReadLine()" som returnerer en streng. Deretter kan du bruke ".Length" metoden for å finne lengden til den innleste strengen.

```C#
Console.WriteLine("Skriv inn en setning:");
string setning = Console.ReadLine();
int lengde = setning.Length;
Console.WriteLine("Lengden til strengen er: " + lengde);
```
Output: Lengden til strengen er: 12 (hvis brukeren skrev inn "Hei, dette er en setning")

Ved å bruke ".Length" metoden, kan du også finne lengden til strenger som er lagret i en liste (List) eller et array (Array). For å gjøre dette, må du først hente ut elementet fra listen eller arrayet, og deretter bruke ".Length" metoden.

```C#
List<string> frukt = new List<string>() {"eple", "banan", "kiwi"};
int lengde = frukt[1].Length;
Console.WriteLine("Lengden til strengen er: " + lengde);
```
Output: Lengden til strengen er: 5 (da "banan" ble hentet fra listen)

## Dykk dypere

I C# er en streng en samling av tegn som er lagret i en array. Når du bruker ".Length" metoden, returnerer den egentlig antall elementer i arrayet, som tilsvarer lengden til strengen. Det kan være nyttig å forstå denne bakgrunnen for å bedre forstå hvordan metoden fungerer.

Det er også verdt å merke seg at "lengden" til en streng kan være annerledes enn antall bokstaver i strengen. Dette skyldes at visse spesialtegn og emoji bruker flere byte og derfor telles som flere elementer i arrayet.

For å håndtere dette, kan man bruke metoden ".ToCharArray" som konverterer strengen til en array av tegn. Deretter kan man bruke ".Length" metoden til å finne antall bokstaver i strengen.

## Se også

- [C# String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [C# String.ToCharArray Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tochararray?view=net-5.0)
- [C# Arrays and Lists](https://www.c-sharpcorner.com/article/arrays-and-lists-in-c-sharp/) (på engelsk)