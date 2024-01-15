---
title:                "Å finne lengden til en streng"
html_title:           "C#: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Strenger er en viktig del av programmering og brukes til å lagre og manipulere tekst. Det å finne lengden av en streng er et vanlig problem som kan oppstå når man jobber med tekstbehandling eller dataanalyse.

## Hvordan gjøre det

Det fins flere måter å finne lengden av en streng på i C#. Her er to eksempler:

```C#
// Eksempel 1: Using the .Length method
string text = "Hei, verden!";
int length = text.Length;

Console.WriteLine(length); // Output: 13
```

```C#
// Eksempel 2: Using a for loop
string text = "Hello, world!";
int length = 0;

for (int i = 0; i < text.Length; i++)
{
    length++;
}

Console.WriteLine(length); // Output: 13
```

I det første eksempelet bruker vi den innebygde `.Length` metoden som finnes på alle strenger. Denne returnerer antall tegn i strengen, inkludert mellomrom og spesialtegn.

I det andre eksempelet bruker vi en `for`-løkke til å telle antall tegn i strengen. Vi starter på indeks 0 og øker tellevariabelen for hver iterasjon. Når løkken er ferdig, har vi telt alle tegnene og kan skrive ut resultatet.

## Dypdykk

I C# er en streng en samling av tegn som lagres som en rekke med `char`-verdier. Derfor kan vi bruke egenskapen `.Length` eller en `for`-løkke for å telle disse verdiene og dermed få lengden på strengen.

Det er viktig å merke seg at `.Length` returnerer antall tegn, ikke antall ord i strengen. Hvis du ønsker å telle ord, kan du bruke `text.Split(' ').Length`, som splitter strengen ved mellomrom og returnerer antall elementer i den resulterende arrayen.

En annen ting å være oppmerksom på er at `.Length` ikke tar hensyn til hvilket språk strengen er i. For eksempel vil ordet "hello" anses som fem tegn uavhengig av om språket er engelsk eller norsk.

## Se også

- [Microsoft dokumentasjon: String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [C# String Methods](https://www.w3schools.com/cs/cs_ref_string.asp)