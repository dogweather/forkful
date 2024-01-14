---
title:    "Elm: Läsning av kommandoradsargument"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Om du är en utvecklare som använder Elm för frontend-utveckling, har du kanske hört talas om att läsa kommandoradsargument. Att kunna läsa och använda kommandoradsargument kan hjälpa till att effektivisera din kod och ge dig mer kontroll över hur din applikation beter sig. I denna bloggpost kommer vi att utforska varför det är användbart och hur man gör det i Elm.

## Så här gör du

För att läsa kommandoradsargument, behöver du först importera "Elm-Kernel"-paketet:

``` Elm-Kernel.Command
import
  Kernel.Command exposing (args)
```

Sedan kan du använda funktionen "args" för att läsa och hantera argumenten som skickas till din Elm-applikation via kommandoraden. Detta ger dig möjlighet att tilldela argumenten till variabler och använda dem i din kod. Här är ett enkelt exempel:

``` Elm
printArguments : Cmd msg
printArguments =
    Cmd.map (\arguments -> Debug.log "Arguments: " arguments) Kernel.Command.args
```

När du kör denna kod och skickar in kommandoradsargument som "elm-make Main.elm --name=John", kommer du att se utskriften "Arguments: [("--name", "John")]" i din debuggare.

## Djupdykning

Förutom enkla argument som i exemplet ovan, kan du även läsa och hantera mer komplexa argument, som flaggor och värden med hjälp av "elm optparse-applicative"-paketet. Detta paket gör det enklare att läsa och tolka argument från kommandoraden.

Det är också viktigt att notera att kommandoradsargument är föränderliga, vilket innebär att du kan läsa argument i ett tidigt skede av uppstarten och senare uppdatera deras värden baserat på andra händelser eller användarinmatning.

## Se även

- [Elm-Docs - Command Module](https://package.elm-lang.org/packages/elm/core/latest/Kernel-Command)
- [Elm-Docs - elm optparse-applicative Package](https://package.elm-lang.org/packages/Skinney/elm-optparse-applicative/latest/)
- [Elm-Sverige Forum - Diskussion om Kommandoradsargument](https://discourse.elm-sverige.nu/t/lasa-kommandoradsargument-i-elmdos-apps/537)