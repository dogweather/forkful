---
title:    "Elixir: Användning av reguljära uttryck"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Regular expressions är en kraftfull funktion inom Elixir som används för att matcha och manipulera textsträngar. Genom att behärska regular expressions kan du effektivt och snabbt utföra sökningar och ersättningar i större textfiler. Det sparar tid och gör dina kodningar mer effektiva.

## Hur man gör

För att använda regular expressions inom Elixir behöver du först importera modulen Regex med `import Regex`. Sedan kan du skriva uttryck inom en `~r` eller `~R` tagg för att matcha en textsträng. Här är ett enkelt exempel på hur du kan matcha en viss bokstav:

```
Elixir ~r/a/  #=> Returns a match for a letter 'a'

Elixir ~R/a/  #=> Returns a match for any occurence of the letter 'a'
```

Du kan också använda metoder som `match?` och `match` för att utföra sökningar och få tillbaka resultat. Om du exempelvis vill hitta en email-adress i en textsträng kan du använda `match?` funktionen:

```
Elixir email = ~r/^[\w+\-.]+@[a-z\d\-]+(\.[a-z]+)*\.[a-z]+$/i
Elixir match?(email, "example@gmail.com") #=> Returns true
Elixir match?(email, "invalid_email") #=> Returns false
```

## Djupdykning

Regular expressions innehåller många olika metakaraktärer och modifierare som kan hjälpa dig att göra mer avancerade sökningar och ersättningar. Här är några vanliga exempel:

- `^` betyder att uttrycket måste matcha från början av en textsträng
- `$` betyder att uttrycket måste matcha till slutet av en textsträng
- `+` betyder att uttrycket måste matcha en eller flera gånger
- `*` betyder att uttrycket kan finnas eller inte och kan förekomma flera gånger
- `?` betyder att uttrycket antingen kan finnas eller inte

Det finns också många användbara modifierare som `i` för att ignorera skillnader mellan stora och små bokstäver, `m` för att matcha flera rader och `s` för att ignorera line breaks.

Det är också viktigt att notera att regler för regular expressions kan vara olika inom olika programmeringsspråk, så det är bra att kontrollera syntaxen och modifierarna för just den programmering du arbetar med.

## Se också

Här är några användbara resurser för att lära dig mer om regular expressions:

- [Elixir Regex dokumentation](https://hexdocs.pm/elixir/Regex.html)
- [Elixir Regex cheat sheet](https://devhints.io/elixir-regex)
- [Regular expression-tutorial på svenska](https://code.tutsplus.com/sv/tutorials/an-introduction-to-regular-expressions--net-5731)