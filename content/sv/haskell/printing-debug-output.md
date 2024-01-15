---
title:                "Utskrift av felsökningsresultat"
html_title:           "Haskell: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva felsökningsutskrift är ett viktigt verktyg för att förstå vad som händer i vår kod. Genom att skriva ut värden på olika variabler och delar av programflödet kan vi se exakt vad som händer vid varje steg och hitta eventuella fel eller förbättringsområden.

## Hur man gör

För att skriva ut felsökningsutskrift i Haskell kan vi använda oss av funktionen "print". Den tar ett värde som parameter och skriver ut det till skärmen. Låt oss till exempel säga att vi har en variabel "number" med värdet 42. Genom att skriva:
```Haskell
print number
```
kommer värdet 42 att skrivas ut till skärmen.

En annan användbar funktion är "trace" från "Debug.Trace" biblioteket. Den kan användas för att skriva ut förklarande text tillsammans med värden. Till exempel:
```Haskell
import Debug.Trace (trace)

let x = 10
let y = 5
trace ("x = " ++ show x) (x + y) 
```
skriver ut "x = 10" och returnerar sedan värdet 15.

## Djupdykning

När vi printar värden som är av en komplexare typ, som en lista eller en tuple, kan vi använda oss av "show" funktionen för att få en mer läsbar utskrift. Till exempel:
```Haskell
let myTuple = (1, "Hello", True)
print $ show myTuple
```
skriver ut "(1, "Hello", True)" istället för det mer svårlästa "(1,"Hello",True)". 

Vi kan också använda oss av "putStrLn" för att skriva ut flera värden på samma rad. Till exempel:
```Haskell
putStrLn "My favorite color is" ++ " blue!"
```
skriver ut "My favorite color is blue!".

Det är även möjligt att använda felsökningsutskrift med hjälp av monader, som "IO". Genom att använda "putStrLn" och >>= operatorn kan vi printa värden inuti våra monader.

## Se även

- [Haskell Debugging Techniques](https://wiki.haskell.org/Debugging_techniques)
- [Debugging in Haskell with trace](https://stackify.com/haskell-debugging-with-trace/)