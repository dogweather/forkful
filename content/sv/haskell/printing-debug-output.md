---
title:                "Haskell: Utskrift av felsökningsutmatning"
simple_title:         "Utskrift av felsökningsutmatning"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

##Varför
I Haskell-utveckling kan det ibland vara användbart att kunna skriva ut debug-utmatning för att förstå hur ens kod arbetar och var det kan finnas eventuella problem. I denna bloggpost ska vi titta på hur man enkelt kan använda sig av debug-utmatning i Haskell.

##Så här gör du
Att skriva ut debug-utmatning i Haskell är enkelt med hjälp av funktionen `traceShow`. Den tar emot ett värde och en sträng och skriver ut strängen tillsammans med värdet. Här är ett enkelt exempel:

```Haskell
import Debug.Trace

main = do
  let x = 5
      y = 10
      z = traceShow (x + y) "Summan av x och y är:"
  print z
```
Output: Summan av x och y är: 15

Som du kan se skrevs strängen ut tillsammans med värdet `15`. Man kan också använda operatorn `.` för att kombinera flera `traceShow` anrop. Här är ett annat exempel:

```Haskell
import Debug.Trace

main = do
  let x = 5
      y = 10
      z = traceShow (x+y) . traceShow (x*y) $ "Summan av x och y är:"
  print z
```
Output: Summan av x och y är: 15

Som du kan se skrevs både summan och produkten av `x` och `y` ut tillsammans med strängen.

##Djupdykning
I Haskell är det viktigt att notera att `traceShow` är en ren funktion, vilket innebär att den alltid returnerar samma värde för samma indata. Detta innebär att trots att du använder `traceShow` för att skriva ut värden när de beräknas, så kommer dessa värden inte att skrivas ut om de samma värden används senare i din kod.

Detta kan vara förvirrande för många utvecklare, särskilt de som är vana vid att skriva ut debug-utmatning i andra språk på samma sätt. Det är viktigt att vara medveten om detta beteende och förstå att `traceShow` bara är till för att hjälpa dig att förstå hur värdena beräknas.

##Se även
- [Haskell Debugging Techniques](https://mmhaskell.com/blog/2019/3/25/haskell-debugging-techniques)
- [Debugging in Haskell](https://www.fpcomplete.com/blog/2017/01/programming-haskell-debugging/)