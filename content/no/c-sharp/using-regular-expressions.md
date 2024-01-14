---
title:                "C#: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regular expressions, eller regulære uttrykk, er en svært kraftig funksjon i programmering. Det lar oss søke, analysere og manipulere tekst på en mer avansert måte enn ved å bare bruke vanlige strenger.

## Hvordan

For å bruke regulære uttrykk i C#, må du først importere System.Text.RegularExpressions biblioteket. Deretter kan du bruke Regex-klassen til å definere og utføre søk. La oss se på et eksempel:

```C#
string input = "Hei, mitt navn er Anna. Jeg er 25 år gammel.";
string pattern = "navn (.+). (.*)år gammel";

// Oppretter et nytt Regex-objekt og bruker det til å søke i Input-teksten
Regex regex = new Regex(pattern);
Match match = regex.Match(input);

// Henter ut resultatet ved hjelp av Group-metoden
string name = match.Groups[1].Value;
int age = Int32.Parse(match.Groups[2].Value);

// Printer ut resultatet
Console.WriteLine("Hei, {0}. Du er {1} år gammel.", name, age);
```

I dette eksempelet bruker vi et regulært uttrykk til å finne navnet og alderen til en person i en tekststreng. Først definerer vi søke-mønsteret vårt, som består av to grupper: "navn" etterfulgt av alt som kommer etterpå, og "år gammel" etterfulgt av et tall. Deretter bruker vi Regex-klassen til å utføre søket og henter ut resultatet ved hjelp av Group-metoden.

Her er hva koden vår skal produsere som output:

```
Hei, Anna. Du er 25 år gammel.
```

## Dypdykk

Regulære uttrykk kan virke forvirrende og vanskelige å forstå i begynnelsen, men med litt øving vil du mestre dem på kort tid. Her er noen nøkkelkonsepter å huske på når du jobber med regulære uttrykk:

- "." matcher ethvert tegn unntatt ny linje
- "^" markerer starten på en linje, mens "$" markerer slutten på en linje
- "\w" matcher et hvilket som helst alfanumerisk tegn, mens "\d" matcher et hvilket som helst tall
- "[]" matcher et tegn fra en gitt karakterklasse, for eksempel [a-z] for alle små bokstaver
- "+" matcher et eller flere forekomster av det forutgående uttrykket
- "*" matcher null eller flere forekomster av det forutgående uttrykket

Ta deg tid til å eksperimentere med disse og andre konsepter for å bli mer komfortabel med å bruke regulære uttrykk.

## Se også

- [Microsofts offisielle dokumentasjon om regulære uttrykk i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [RegExr - en interaktiv nettbasert lekeplass for å eksperimentere med regulære uttrykk](https://regexr.com/)