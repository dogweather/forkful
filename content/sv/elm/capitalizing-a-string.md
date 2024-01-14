---
title:    "Elm: Konvertera en sträng till Caps"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
I programmering finns det oftast flera sätt att lösa ett problem, och att välja vilken metod man ska använda beror ofta på personliga preferenser eller på vilket språk man använder. Att kapitalisera en sträng kan utföras på flera olika sätt, men vi ska titta på hur det görs i Elm.

## Så här gör du
För att kapitalisera en sträng i Elm, använd funktionen `String.toUpper`. Nedan följer ett exempel på hur man kan använda den i sin kod:

```Elm
import String exposing (toUpper)

name = "elm is awesome"

capitalizedName = toUpper name

-- Resultat: "ELM IS AWESOME"
```

Som du kan se i exemplet, skapar vi först en variabel `name` och tilldelar den en sträng. Sedan använder vi funktionen `toUpper` för att omvandla strängen till stora bokstäver, och tilldelar resultatet till en ny variabel `capitalizedName`.

## Djupdykning
Förutom `String.toUpper` finns det flera andra inbyggda funktioner i Elm som kan användas för att manipulera strängar. Till exempel kan man använda `String.trim` för att ta bort mellanslag från början och slutet av en sträng, eller `String.reverse` för att vända på en sträng. Det finns också funktioner för att jämföra och konkatenera strängar.

Det är också värt att notera att i Elm är strängar oföränderliga, vilket betyder att man inte kan ändra en sträng direkt utan i stället måste skapa en kopia med de önskade förändringarna.

## Se även
- [Elm Dokumentation](https://elm-lang.org/docs)
- [Elm Official Guide](https://guide.elm-lang.org)
- [Elm String Modul](https://package.elm-lang.org/packages/elm/core/latest/String)