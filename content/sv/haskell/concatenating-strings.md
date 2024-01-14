---
title:    "Haskell: Sammanslagning av strängar"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att konkatenera strängar är en viktig del i programmering eftersom det tillåter oss att kombinera olika bitar av text till en enda sträng. Det här kan vara användbart när vi till exempel vill skapa en textbaserad användargränssnitt eller lägga till variabelvärden i en sträng.

## Så här gör du

För att konkatenera strängar i Haskell, behöver vi använda operatören "++". Det här operatören tillåter oss att kombinera två strängar till en enda sträng. Till exempel:

```Haskell
para :: String
para = "Jag älskar " ++ "att programmera."
```

Det här kommer att ge oss en sträng som ser ut såhär: "Jag älskar att programmera."

## Djupdykning

I Haskell, kan vi även använda "++" operatören för att konkatenera flera strängar samtidigt. Till exempel:

```Haskell
strängar :: [String]
strängar = ["Jag", "älskar", "att", "programmera."]
para :: String
para = concat strängar
```

Här skapar vi en lista med strängar och använder sedan "concat" för att konkatenera dem till en enda sträng. Det här är särskilt användbart när vi vill kombinera ett stort antal strängar på ett effektivt sätt.

## Se även

- [Haskell Wikibooks](https://en.wikibooks.org/wiki/Haskell)
- [Haskell for Dummies](https://www.dummies.com/programming/haskell)
- [Haskell Tutorial](https://www.tutorialspoint.com/haskell/index.htm)