---
title:    "C#: Sammenkobling av strenger"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Å koble sammen strenger er en nyttig teknikk som lar deg kombinere flere variabler eller tekststykker til én streng. Dette er spesielt viktig når du arbeider med tekstbaserte data eller når du må formatere utdata på en spesifikk måte. Ved å bruke denne teknikken, kan du enkelt håndtere komplekse strenger og gjøre koden din mer effektiv.

## Hvordan

For å koble sammen strenger i C#, bruker vi "+" operatøren. La oss se et enkelt eksempel:

```C#
string navn = "Ola";
string alder = "25";

string setning = "Jeg heter " + navn + " og jeg er " + alder + " år gammel.";

Console.WriteLine(setning);
```
**Output:**
>Jeg heter Ola og jeg er 25 år gammel.

Her kombinerer vi variablene "navn" og "alder" med tekststrengene "Jeg heter" og "og jeg er". Vi kan også kombinere mer enn to variabler i samme uttrykk.

```C#
string fornavn = "Kari";
string etternavn = "Nordmann";
int alder = 30;

string setning = "Navnet mitt er " + fornavn + " " + etternavn + " og jeg er " + alder + " år gammel.";

Console.WriteLine(setning);
```
**Output:**
>Navnet mitt er Kari Nordmann og jeg er 30 år gammel.

Merk at vi bruker "+" operatøren for å kombinere variabler og tekststrenger. Det er viktig å huske på at alle variabler må være av samme type, ellers vil koden ikke fungere.

## Deep Dive

I tillegg til "+" operatøren, kan vi også bruke metoden "Format" for å kombinere strenger i C#. Dette vil gi deg mer kontroll over formateringen av utdata.

```C#
string fornavn = "Per";
string etternavn = "Hansen";
int id = 1234;

string setning = string.Format("Min ID er {2} og mitt navn er {0} {1}.", fornavn, etternavn, id);

Console.WriteLine(setning);
```
**Output:**
>Min ID er 1234 og mitt navn er Per Hansen.

Som du kan se, bruker vi en "formatteringsstreng" i metoden for å fortelle C# hvor variablene skal plasseres i setningen. Dette kan være spesielt nyttig når du arbeider med mer komplekse utdataformater.

## Se Også

1. [Konkatinering av strenger i C#](https://www.w3schools.com/cs/char_arrays.asp)
2. [C# Strenger](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/index)