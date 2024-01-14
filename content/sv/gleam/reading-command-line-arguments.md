---
title:    "Gleam: Att läsa kommandoradsargument"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa kommandoradsargument är ett viktigt verktyg för alla Gleam-programmerare. Genom att kunna läsa in parametrar som användare ger till ditt program kan du skräddarsy körningen och öka dess flexibilitet.

## Så här
Det finns flera sätt att läsa kommandoradsargument i Gleam, men det enklaste är att använda funktionen  `gleam:io:cli::parse_args()`, som tar in en sträng och returnerar en lista med alla argument. Här är ett enkelt exempel:

```Gleam
args = gleam:io:cli::parse_args("program --param1 value1 --param2 value2")
println(args) // [ "--param1", "value1", "--param2", "value2" ]
```
Som du kan se returneras alla argument som separata strängar, vilket gör det enkelt att hantera dem vidare i ditt program.

Om du vill läsa in värden till specifika parametrar kan du använda funktionen `gleam:io:cli::parse_args_with()`, som tar in en lista med argumentnamn och returnerar en lista med motsvarande värden. Om ett argument inte har ett tillhörande värde kommer `false` att returneras.

```Gleam
[foo, bar] = gleam:io:cli::parse_args_with(["--foo", "--bar"])
println(foo) // true
println(bar) // false
```

## Djupdykning
Det finns många andra användbara funktioner för att läsa kommandoradsargument i Gleam, såsom `gleam:io:cli::parse_args_to_map()` som returnerar en map med alla argument och deras värden. Det finns också möjlighet att läsa in flaggor och argument med liknande namn som ska grupperas tillsammans.

Det är viktigt att komma ihåg att kommandoradsargument alltid är strängar, så om du behöver läsa omvandla dem till andra datatyper måste du göra det manuellt.

## Se även
- [Gleam Dokumentation för Gleam:io:cli modulen](https://gleam.run/documentation/stdlib/io/cli)
- [En guide för att använda kommandoradsargument i Gleam](https://www.joannevy.com/using-command-line-arguments-in-gleam/)