---
title:                "Gör om en sträng till versaler"
aliases:
- sv/elm/capitalizing-a-string.md
date:                  2024-02-03T19:05:19.617947-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att versalisera en sträng innebär att omvandla det inledande tecknet i en given sträng till versal medan resten behålls i gemener, ofta för standardiserad formatering eller läsbarhet. Programmerare utför ofta denna uppgift för att säkerställa att data presenteras konsekvent, särskilt i användargränssnitt eller när man bearbetar och visar användarinmatning.

## Hur gör man:

I Elm finns ingen inbyggd funktion specifikt för att versalisera strängar. Dock kan du enkelt uppnå detta genom att använda de inbyggda funktionerna i `String`-modulen som `toUpper`, `toLower`, `left` och `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Exempelanvändning
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Output: "Hello World"
```

För mer komplexa scenarier eller om du föredrar att använda ett bibliotek som erbjuder ett direkt sätt att versalisera strängar, kan du överväga ett tredjepartspaket som `elm-community/string-extra`. Dock uppmuntrar Elm:s ekosystem, enligt min senaste uppdatering, hantering av sådana uppgifter med inbyggda funktioner för att hålla språket och projekten "lean".

```elm
import String.Extra as StringExtra

-- Om det finns en `capitalize` funktion i ett tredjepartsbibliotek
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Exempelanvändning med hypotetisk biblioteksfunktion
main =
    "this is elm" |> capitalizeWithLibrary
    -- Hypotetisk output: "This is elm"
```

Kontrollera alltid Elm-paketdatabasen för de senaste och mest föredragna biblioteken för strängmanipulering om du letar efter ytterligare funktionalitet bortom standardbiblioteket.
