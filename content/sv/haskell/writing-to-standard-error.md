---
title:                "Haskell: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Varför skriva till standard error i Haskell

Att skriva till standard error är ett viktigt koncept inom Haskell-programmering och kan vara till nytta i många olika situationer. Oavsett om du vill felsöka ditt program eller skriva ut användbar information till användaren, så är skrivning till standard error ett värdefullt verktyg som alla bör ha i sin verktygslåda.

## Så här gör du

Skrivning till standard error i Haskell är enkelt och kan utföras med hjälp av funktionen "hPutStrLn" från modulen "System.IO". Här är ett exempel på hur du kan skriva till standard error:

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Detta är ett felmeddelande."
  hPutStrLn stderr "Försök igen."
```

När du kör detta program kommer det att skriva ut de två meddelandena till standard error, vilket vanligtvis visas i terminalen eller kommandotolken.

## Djupdykning

Att skriva till standard error kan vara särskilt användbart när du vill separera olika typer av utskrifter. Till exempel kan du använda "hPutStrLn" för att skriva felmeddelanden till standard error och "putStrLn" för att skriva vanliga meddelanden till standard out. På så sätt kan du enkelt skilja mellan dem och veta var du ska leta när du felsöker ditt program.

Standard out, eller "stdout", är vanligtvis där alla utskrifter hamnar som standard i Haskell-program. Detta innefattar även användarens input om du använder "getLine" funktionen. Genom att istället skriva till standard error kan du säkerställa att ditt felmeddelande inte blandas med ditt programs vanliga utskrifter.

Även om vi här har använt funktionen "hPutStrLn" för att skriva till standard error, så finns det flera andra funktioner som kan användas för att uppnå samma resultat. Det är bara en fråga om personlig preferens och vad som passar bäst för ditt program.

## Se även

Här är några andra användbara resurser för att lära dig mer om att skriva till standard error i Haskell:

- [Official Haskell documentation on "System.IO" module](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Haskell wiki page on "Streams"](https://wiki.haskell.org/Streams)
- [Tutorialspoint article on "Haskell Files and I/O"](https://www.tutorialspoint.com/haskell/haskell_files_io.htm)

Med dessa resurser och förmågan att skriva till standard error i din verktygslåda, kommer du att kunna skriva robusta och felsökande Haskell-program som är både lättlästa och användarvänliga. Lycka till!