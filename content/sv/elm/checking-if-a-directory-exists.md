---
title:                "Kontrollera om en katalog finns"
html_title:           "Elm: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar är en viktig del av programmering, speciellt om du skapar ett program som behöver hantera filer eller sökvägar. Det är också användbart för att undvika att programmet kraschar om mappen inte finns.

## Såhär gör du
Det finns en inbyggd funktion i Elm som heter `Directory.dirExists`, vilket gör det enkelt att kolla om en mapp existerar eller inte. Här är ett exempel på hur du kan använda den:

```Elm
import Directory

-- Skapa en funktion som tar en sökväg som argument och returnerar en bool som säger om mappen finns eller inte.
directoryExists : String -> Bool
directoryExists path =
    Directory.dirExists path
```

Om vi till exempel vill kolla om mappen "Bilder" existerar, kan vi skriva:

```Elm
directoryExists "Bilder"
--> True
```

Om mappen inte finns, returneras `False` istället.

## Djupdykning
Det finns några saker att tänka på när man använder `Directory.dirExists`:

- Funktionen tar en sträng som argument, vilket innebär att du behöver kolla efter mappar baserat på sökvägen.
- Det är viktigt att säkerställa att du har tillräckliga rättigheter för att kolla efter mappen, annars kommer funktionen att returnera `False`.
- Om mappen du kollar efter innehåller specialtecken, behöver du koda om dem först innan du använder `Directory.dirExists`.

## Se även
- [Elm dokumentation om Directory](https://package.elm-lang.org/packages/elm/core/latest/Directory)
- [Stack Overflow svar på hur man kollar om en mapp existerar i Elm](https://stackoverflow.com/questions/53019146/how-do-i-check-if-a-folder-exists-in-elm/53024682)