---
title:                "Att påbörja ett nytt projekt"
html_title:           "Haskell: Att påbörja ett nytt projekt"
simple_title:         "Att påbörja ett nytt projekt"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att starta ett nytt projekt i Haskell är när du skapar en ny mapp och börjar skriva kod för en ny applikation eller bibliotek. Programmerska utför detta för att implementera nya idéer, lära sig nya tekniker eller lösa ett problem som de står inför.

## Hur man:

Kodexempel och exempel på output visas nedanför i kodblock vilket signaleras med ```Haskell ... ```

```
-- Skapar en ny mapp för projektet
mkdir min_haskell_projekt

-- Skapar en ny fil som heter "Hello.hs"
touch Hello.hs

-- Öppnar filen med din valda textredigerare, i detta fallet 'vi'
vi Hello.hs
```

```Haskell
-- Hello.hs
main :: IO ()
main = putStrLn "Hej världen!"
```

Output:
```
Hej världen!
```

## Djupdykning:

När Haskell skapades på 1980-talet var det en av de första funktionella programmeringsspråken och användes i forskning inom datormatematik. Alternativen till att starta ett nytt projekt i Haskell inkluderar att använda andra språk som till exempel Java, Python eller C++. Implementationen av att starta ett projekt involverar att skapa en skaplig projektmapp och skriva kod i en fil som används för att köra ditt program eller bibliotek.

## Se även:

Här är några användbara länkar om hur man startar ett nytt projekt i Haskell:
- https://www.haskell.org/platform/
- https://wiki.haskell.org/How_to_write_a_Haskell_program
- https://www.fpcomplete.com/blog/2019/08/starting-a-haskell-project-an-opinionated-guide