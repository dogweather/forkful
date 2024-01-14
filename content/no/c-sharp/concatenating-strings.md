---
title:                "C#: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger, også kalt "string concatenation", er en viktig del av å programmere i C#. Det lar deg enkelt kombinere flere strenger for å lage en lengre ny streng. Dette er spesielt nyttig når du trenger å generere dynamiske tekststrenger, for eksempel i en brukergrensesnitt eller når du lagrer data til en fil.

## Hvordan Du Gjør Det

For å utføre string concatenation i C#, kan du bruke "+" operatøren. Denne operatøren brukes til å kombinere to eller flere strenger, og resulterer i en ny lengre streng.

La oss se på et eksempel:

```C#
string fornavn = "Jens";
string etternavn = "Hansen";
string fulltnavn = fornavn + " " + etternavn; // fulltnavn vil nå være "Jens Hansen"
```

Som du kan se i koden over, bruker vi "+" operatøren til å kombinere fornavn, et mellomrom og etternavn for å lage en ny streng som inneholder fullt navn.

Du kan også bruke denne operatøren til å legge til andre variabler eller tall til en streng. For eksempel:

```C#
string alder = "34";
string beskjed = "Jeg er " + alder + " år gammel."; // beskjed vil være "Jeg er 34 år gammel."
```

Når du kombinerer en streng med et tall, vil C# automatisk konvertere tallet til en streng før de kombineres.

## Dykk Dypere

I tillegg til "+" operatøren, kan du også bruke "String.Format()" metoden for å kombinere strenger i C#. Dette er spesielt nyttig når du trenger å formatere teksten din på en bestemt måte, for eksempel når du lager en rapport.

```C#
string beskjed = String.Format("Hei, mitt navn er {0} og jeg er {1} år gammel.", fornavn, alder);
// beskjed vil være "Hei, mitt navn er Jens og jeg er 34 år gammel."
```

Dette gjør det enkelt å bytte ut variabler i teksten din og sikrer at de er i riktig rekkefølge.

## Se Også

- [MSDN: String Concatenation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#concat)
- [Tutorialspoint: C# String Concatenation](https://www.tutorialspoint.com/csharp/csharp_string_concatenation.htm)
- [Programmeringsfora C#: String Concatenation Tutorial](https://www.programmeringsfora.com/tutorials/csharp/string-concatenation)