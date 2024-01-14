---
title:    "Elm: Extrahering av substrängar"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I programmering finns det ofta behov av att manipulera textsträngar, dvs. en sekvens av tecken som bildar en text. Ibland kan det vara nödvändigt att extrahera delar av denna sträng, vilket kan utföras genom att använda funktioner som utvinnar substrängar. I denna artikel kommer vi att ta en titt på hur man avlägsnar substrängar i Elm programmeringsspråket.

## Hur man gör

För att avlägsna en substräng i Elm kan man använda funktionen `slice`, som tar tre argument: en startposition, en slutposition och en sträng. Här är ett exempel på hur man skulle kunna extrahera en del av en sträng:

```Elm
sträng = "Hej världen!"

del = sträng
    |> String.slice 4 9

```

I detta exempel har vi en sträng som heter "Hej världen!" och vi använder sedan `String.slice` för att få en del av den, från position 4 till position 9 (vilket ger oss "världen"). Detta kan vara användbart när man behöver bearbeta en del av en större sträng, t.ex. om man vill hämta ut ett specifikt ord ur en lång text.

## Djupgående

Funktionen `slice` är bara en av många användbara funktioner för stränghantering som finns tillgängliga i Elm. Det finns också andra funktioner som t.ex. `startsWith`, `endsWith`, `contains` och `indexOf` för att hantera substrängar på olika sätt. Man kan också använda regex (reguljära uttryck) för att mer avancerat söka och extrahera delar av strängar.

Utöver det innefattar Hantera strängar i Elm också bearbetning av teckenkodning, hantering av specialtecken och konvertering mellan olika typer av data som t.ex. strängar och listor. Det är viktigt att förstå dessa funktioner och koncept för att effektivt kunna hantera text i sina Elm-program.

## Se även

- Official Elm documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Användbara funktioner för stränghantering i Elm: https://guide.elm-lang.org/strings/
- Reguljära uttryck (regex) i Elm: https://package.elm-lang.org/packages/elm/regex/latest/