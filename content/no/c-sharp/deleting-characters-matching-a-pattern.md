---
title:    "C#: Slette karakterer som matcher et mønster"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering, må vi manipulere data for å få ønsket resultat. Noen ganger må vi også slette visse tegn eller mønstre fra en streng. Dette kan være nyttig for å rense og forberede data for videre behandling. I denne bloggposten skal vi se på hvordan du kan slette tegn som matcher et gitt mønster i programmeringsspråket C#.

## Hvordan

Det finnes flere måter å slette tegn som matcher et mønster på i C#. I følgende kodeblokk vil vi bruke `Regex.Replace()`-metoden for å slette alle tall fra en streng.

```C#
string input = "abc123def456ghi789";
string output = Regex.Replace(input, @"\d", ""); //output: abcdefghi
```

I dette eksempelet bruker vi `\d` som mønster, som representerer alle tall. Her erstatter vi alle tall med en tom streng, og får dermed en ny streng uten tall.

Vi kan også bruke `Regex.Replace()`-metoden til å slette et bestemt mønster fra en streng. I følgende eksempel vil vi slette alle vokaler fra en streng.

```C#
string input = "Hello World!";
string output = Regex.Replace(input, "[aeiou]", ""); //output: Hll Wrld!
```

Her bruker vi `[aeiou]` som mønster, som representerer alle vokaler. Ved å erstatte disse med en tom streng, får vi en ny streng uten vokaler.

## Deep Dive

Når vi bruker `Regex.Replace()`-metoden, kan vi også spesifisere et tredje parameter, kalt `count`, som definerer hvor mange forekomster av mønsteret vi ønsker å erstatte. I følgende eksempel vil vi kun erstatte de to første forekomstene av mønsteret "oo" i en streng.

```C#
string input = "foo bar baz boo";
string output = Regex.Replace(input, "oo", "", 2); //output: f bar baz bo
```

I tillegg til `Regex.Replace()`, har C# også en innebygd `Replace()`-metode for strenger som kan brukes til å slette et mønster. Denne metoden tar inn en `char` eller en `string` som parametere, i stedet for et regulært uttrykk.

## Se Også

- [Microsoft dokumentasjon for Regex.Replace()](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=netcore-3.1)
- [C# Replace-metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)