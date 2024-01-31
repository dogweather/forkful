---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) är att skicka felmeddelanden och diagnostik separat från huvudprogrammets output (stdout). Det är viktigt för att kunna spåra fel smidigt och hantera loggar effektivt.

## Hur gör man:
Haskell kod använder `System.IO`-modulen för att hantera stderr. Använd `hPutStrLn` tillsammans med `stderr`.

```Haskell
import System.IO (stderr, hPutStrLn)

main = do
  -- Skriv ut ett normalt meddelande till stdout
  putStrLn "Detta är ett meddelande till standard output."

  -- Skriv ut ett felmeddelande till stderr
  hPutStrLn stderr "Detta är ett felmeddelande till standard error."
```

Testa genom att omdirigera output i terminalen:

```bash
runhaskell dittprogram.hs > output.txt 2> error.log
```

`output.txt` kommer innehålla "Detta är ett meddelande till standard output." och `error.log` kommer ha "Detta är ett felmeddelande till standard error."

## Djupdykning
Historiskt sätt separeras stdout och stderr för att erbjuda en ren output-ström för data och en annan för fel. Alternativ inkluderar att logga till en fil eller använda externa bibliotek för avancerad logghantering. Intern implementation i Haskell använder de lågnivåfunktioner som operativsystemet tillhandahåller för att skriva till dessa strömmar.

## Se även
- Haskell-dokumentationen för `System.IO`: https://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html
- Bra genomgång av stdout vs stderr: https://www.jstorimer.com/blogs/workingwithcode/7766119-when-to-use-stderr-instead-of-stdout
- Detaljerad guide till loggning i Haskell: https://ocharles.org.uk/posts/2012-12-04-24-days-of-hackage-hslogger.html
