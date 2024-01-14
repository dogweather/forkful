---
title:                "Haskell: Skrivande till standardfel"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är ett viktigt verktyg för att diagnostisera och felsöka programmeringsfel. Det gör det möjligt för oss att skriva ut meddelanden som visar var i koden ett fel uppstod och vilken typ av fel det är. Detta hjälper oss att förbättra våra program och undvika potentiella buggar.

## Hur man gör det

För att skriva till standard error i Haskell kan du använda funktionen `hPutStrLn` från `System.IO`-modulen. Den tar som argument standard error-ström (`stderr`) och en sträng som ska skrivas ut. Här är ett enkelt exempel:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "Detta är ett meddelande till standard error."
```

Detta kommer att skriva ut "Detta är ett meddelande till standard error." till standard error-strömmen när programmet körs. Notera att du också kan använda `hPutStr` för att skriva ut en sträng utan ett nyradstecken.

Du kan också använda `putStrLn` och `putStr` för att skriva ut till standard output (`stdout`). Skillnaden är att `stdout` är den vanliga utmatningsströmmen medan `stderr` är avsett för felmeddelanden. Genom att skriva till rätt ström kan du se till att dina meddelanden visas på rätt ställe och inte blandas ihop med programutmatningen.

## Deep Dive

Standard error-strömmen är en del av tre standardströmmar i ett Haskell-program: `stdin` för inmatning, `stdout` för utmatning och `stderr` för felmeddelanden. Det är viktigt att förstå skillnaden mellan dessa strömmar och när det är lämpligt att använda dem.

När du kör ett Haskell-program från terminalen är `stdout` standardutmatningen och `stderr` standardfelmeddelanden. Detta innebär att alla meddelanden som skrivs till `stdout` kommer att visas i terminalen, medan meddelanden som skrivs till `stderr` kommer att visas som röda felmeddelanden. Detta gör det enkelt för användare att skilja mellan programutmatning och felmeddelanden.

Att använda funktionerna i `System.IO`-modulen ger dig också kontroll över vilken ström du skriver till och var du skriver den. Det finns tillfällen när det är bättre att skriva till `stdout` istället för `stderr`, till exempel när du skriver ut information om hur programmet kör. Detta kan göra din programutmatning mer lättläst och lätt att följa.

## Se även

- [Haskell IO](https://wiki.haskell.org/IO)
- [Haskell Standard I/O](https://www.haskell.org/tutorial/io.html)
- [System.IO modulen](https://hackage.haskell.org/package/base/docs/System-IO.html)