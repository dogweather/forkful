---
title:                "Haskell: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en vanlig uppgift i programmering. Med hjälp av Haskell kan du lära dig en enkel och effektiv metod för att göra det.

## Så här gör du
Först behöver vi en sträng att arbeta med. Låt oss anta att vi har en variabel `text` som innehåller strängen "Hej världen!".

Vi kan använda Haskells `length` funktion för att hitta längden på strängen:

```Haskell
length text
```

Detta ger oss ett resultat på 13, vilket är antalet tecken i strängen.

Om vi vill skriva ut längden på strängen tillsammans med själva strängen kan vi använda `show` funktionen tillsammans med `length`:

```Haskell
show (length text) ++ " " ++ text
```

Detta ger oss följande output:

```
"13 Hej världen!"
```

En annan metod för att hitta längden på en sträng är att använda `Data.List` modulen, som innehåller en funktion som heter `genericLength`. Denna funktion kan hantera både listor och strängar och ger oss samma resultat som `length`:

```Haskell
import Data.List

genericLength text
```
Detta ger oss också ett resultat på 13.

## Deep Dive
Vad är egentligen skillnaden mellan `length` och `genericLength`? Skillnaden ligger i datatypen som funktionerna hanterar. `length` är specifik för listor medan `genericLength` är mer allmän och kan hantera flera olika typer av data, inklusive listor och strängar.

Det är också värt att notera att både `length` och `genericLength` returnerar en `Int` i Haskell. Detta innebär att funktionerna inte är lämpliga för att hantera väldigt långa strängar, eftersom `Int` har en övre gräns på sin storlek.

Om du behöver hantera väldigt långa strängar bör du istället använda dig av funktionen `genericLength` i `Data.List` modulen, eftersom denna funktion returnerar en `Integer` som har en mycket större övre gräns.

## Se också
- Haskells `length` funktion dokumentation: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:length
- Haskells `genericLength` funktion dokumentation: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:genericLength