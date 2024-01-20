---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konkatenera strängar innebär att sammanfoga två eller flera strängar till en. Programmerare gör det för att manipulera data, bygga fram dynamiskt innehåll och formatera output.

## Hur till:

Här är några grunderna för hur du kan konkatenera strängar i Haskell med hjälp av '++' operatören.

```Haskell
let str1 = "Hej "
let str2 = "världen"
let ray = str1 ++ str2
print(ray)
```

Output:

```Haskell
"Hej världen"
```

En annan metod skulle vara använder `concat`-funktionen.

```Haskell
let strList = ["Hej ", "världen"]
let ray = concat strList
print(ray)
```

Output:

```Haskell
"Hej världen"
```

## Djup dykning:

Historiskt sett har Haskell alltid använt funktionell programmering för strängmanipulation. Att konkatenera strängar i Haskell är ingen undantag.

Alternativ till '++' och `concat` inkluderar `mconcat`, som är en utökning av `concat`, och möjliggör mer komplex strängsammansättning.

Under ytan använder Haskell "lata listor" för intern representation av strängar. Varje sträng är en lista av tecken, och konkatenering orsakar en ny lista att skapas, vilket kopierar de gamla listorna. Detta är inte det mest prestanda effektiva sättet att implementera strängar, men Haskell prioriterar renhet och enkelhet framför prestanda.

## Se också:

För ytterligare studier, se följande länkar:
1. Haskells officiella dokumentation om listor: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html
2. En djupare titt på Haskell's 'Lazy Evaluation': http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers-lazy.html
3. Utforska andra strängalternativ som Text och ByteSträng: https://wiki.haskell.org/Performance/Strings