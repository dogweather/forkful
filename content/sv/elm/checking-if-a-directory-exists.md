---
title:                "Elm: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar är en viktig del av programmering för att säkerställa korrekt hantering av filer och resurser. I denna bloggpost kommer vi att utforska hur man kan göra detta i Elm.

## Hur man gör
För att kontrollera om en mapp existerar i Elm, behöver vi använda File paketet som tillhandahåller funktioner för att hantera filer och mappar. Vi kommer att använda funktionen `directoryExists`, som tar en sökväg till mappen som argument och returnerar en `Task Bool` som representerar om mappen existerar eller inte.

```Elm
import File exposing (directoryExists)

showDirectory existence =
  if existence then
    "Mappen finns"
  else
    "Mappen finns inte"

main =
  directoryExists "path/to/directory"
    |> Task.andThen showDirectory
    |> Task.attempt (\_ -> "Något gick fel")
```

Om sökvägen till mappen är giltig och mappen existerar kommer funktionen `showDirectory` att returnera `"Mappen finns"`, annars kommer den att returnera `"Mappen finns inte"`. Vi använder funktionen `Task.attempt` för att hantera eventuella fel som kan uppstå vid anropet av `directoryExists`.

Output (för en existerande mapp):
```
Mappen finns
```

Output (för en icke-existerande mapp):
```
Mappen finns inte
```

## Djupdykning
Det är viktigt att notera att `directoryExists` inte kontrollerar om en mapp är tom eller har specifika filer i sig, den endast kontrollerar om mappen som sådan existerar på den angivna sökvägen. Om du vill kontrollera om en mapp har filer i sig kan du använda funktionen `directoryListing` som returnerar en `Task (List FileInfo)` som innehåller information om alla filer i mappen.

En annan viktig sak att tänka på är att funktionen `directoryExists` inte är asynkron, vilket innebär att den kommer att blockera den körande tråden tills kontrollen är klar. Om detta är ett problem för din applikation kan du använda funktionen `directoryExistsAsync` som tar en time-out som argument och returnerar en `Task (Maybe Bool)` som representerar om mappen existerar, `Just True` om mappen existerar, `Just False` om mappen inte existerar och `Nothing` om det inte var möjligt att avgöra p.g.a. time-out.

## Se också
- [File paketet](https://package.elm-lang.org/packages/elm/file/latest/)

- [Elm dokumentation för Task och asynkron programmering](https://guide.elm-lang.org/effect_managers/)