---
title:                "Interpolering av en streng"
html_title:           "C#: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Interpolering av strenger er en måte å sette sammen variabler og tekst på en enkel og effektiv måte i koden din. Det er spesielt nyttig når du vil opprette dynamisk tekst basert på variabler eller data. Programmerere bruker dette for å gjøre koden deres mer lesbar og for å redusere antall linjer med kode som trengs for å opprette tekst.

# Hvordan:
```C#
string name = "Sofia";
int age = 25;
Console.WriteLine($"Hei, jeg heter {name} og er {age} år gammel.");
```
Output: Hei, jeg heter Sofia og er 25 år gammel.

# Deep Dive:
Interpolering av strenger har eksistert siden C# 6 og er en mer effektiv måte å formatere tekst enn konkatenering. Alternativt kan du bruke metoder som String.Format() eller StringBuilder for å opprette tekst, men interpolering er mer populært blant programmerere på grunn av sin enkelhet og lesbarhet. Når du bruker interpolering, kan du også utnytte funksjoner og metoder direkte i strengen.

# Se også:
- [Offisiell dokumentasjon om interpolering av strenger i C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [Enkel guide til C# interpolering av strenger](https://www.c-sharpcorner.com/article/what-is-string-interpolation-in-C-Sharp/)