---
title:                "Sammanfogning av strängar"
html_title:           "Elixir: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Innan vi hoppar in i hur man konkatenaterar strängar i Elixir, låt oss först förstå varför vi skulle vilja göra det. Att konkatenatera strängar är ett viktigt koncept inom programmering eftersom det ger oss möjlighet att sammanfoga flera delar av en text eller ett meddelande till en enda enhet. Det kan vara användbart för att skapa dynamiska meddelanden eller för att manipulera befintliga strängar på ett mer effektivt sätt.

## Hur man konkatenaterar strängar

För att konkatenatera strängar i Elixir kan vi använda oss av operatorn `<>`. Detta operatorn tar två strängar och sammanfogar dem till en enda med den första strängen först, följt av den andra strängen.

```Elixir
"Hello " <> "world!" #=> "Hello world!"
```

Vi kan också använda funktionen `String.concat/2` som tar ett godtyckligt antal strängar som argument och sammanfogar dem tillsammans.

```Elixir
String.concat("Welcome", "to", "Elixir") #=> "Welcome to Elixir"
```

Om vi vill konkatenatera en lista av strängar kan vi använda funktionen `Enum.join/2` som tar en lista och ett valfritt separator-tecken som argument.

```Elixir
Enum.join(["Programming", "is", "fun"], " ") #=> "Programming is fun"
```

Vi kan också använda funktionen `IO.puts/2` för att skriva ut våra konkatenaterade strängar direkt till konsolen.

```Elixir
IO.puts("Hello " <> "Elixir!") #=> Hello Elixir!
```

## Djupdykning

Det är viktigt att notera att när man konkatenaterar strängar i Elixir skapas en ny sträng istället för att ändra den befintliga strängen. Detta innebär att vi kan vara säkra på att vårt ursprungliga värde inte förändras.

En annan funktion som kan vara användbar för att manipulera strängar är `String.replace/4` som låter oss ersätta en del av en sträng med en annan sträng. Detta kan visa sig användbart när man vill byta ut delar av en text med en dynamiskt genererad sträng.

```Elixir
String.replace("Elixir är ett", "Elixir", "Språk") #=> "Språk är ett"
```

## Se även

- [Elixir Dokumentation om strängar](https://hexdocs.pm/elixir/String.html)
- [Elixir Koans: Strings](https://elixirkoans.org/#strings)