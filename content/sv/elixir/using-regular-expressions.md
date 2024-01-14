---
title:    "Elixir: Att använda vanliga uttryck"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regular expressions (regex) är ett användbart verktyg inom Elixir programmering för strängmanipulation. Det låter dig hitta, matcha och ersätta specifika mönster inom en sträng.

## Så här gör du:
För att använda regex inom Elixir, är det viktigt att använda den inbyggda `Regex` modulen. Det finns flera olika funktioner som du kan använda för att skapa och manipulera regex.

```Elixir
# För att skapa ett regex uttryck
regex = ~r/hello/

# För att söka efter ett visst mönster inom en sträng
Regex.match?(regex, "hello world")

# För att ersätta en del av en sträng med ett annat värde
Regex.replace("Hello World", ~r/World/, "Elixir") #=> "Hello Elixir"
```

## Djupdykning:
Det finns många olika specialtecken som kan användas för att skapa mer avancerade regex uttryck. Till exempel, `^` matchar början av en sträng och `$` matchar slutet av en sträng. Det finns också många olika modifierare, såsom `i` som gör uttrycket skiftlägesobestämt, och `s` som gör att `.` också matchar radbrytningar.

Du kan också använda grupparanteser för att söka efter specifika mönster och extrahera dem från en sträng. Till exempel, `(~r/([0-9]+)-([a-z]+)/)` kommer att extrahera alla tal före ett bindestreck och alla bokstäver efter bindestrecket.

Det finns också ett antal Elixir inbyggda funktioner som kan användas tillsammans med regex, såsom `String.match?` och `String.replace`.

## Se även:
- [Elixir Regex dokumentation](https://hexdocs.pm/elixir/Regex.html)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Learn Elixir - Regular Expressions](https://learn-elixir.dev/docs/regular-expressions)