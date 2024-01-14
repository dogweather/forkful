---
title:    "Haskell: Söka och ersätta text"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

#Varför

Att söka och byta ut text är en vanlig uppgift inom programmering, speciellt när man arbetar med textbaserade filer eller hanterar stora mängder data. Genom att lära sig hur man effektivt söker och ersätter text i Haskell kan du spara tid och göra dina program mer robusta.

##Så här gör du

Haskell har flera inbyggda funktioner som gör det enkelt att söka och byta ut text i strängar. Här är några viktiga funktioner att känna till:

```Haskell
-- Returnerar en sträng där alla förekomster av ett givet tecken ersätts med ett annat tecken
replace :: Char -> Char -> String -> String
replace old new = map (\x -> if x == old then new else x)

-- Returnerar en sträng där alla förekomster av en given sträng ersätts med en annan sträng
replaceAll :: String -> String -> String -> String
replaceAll old new = intercalate new . splitOn old

-- Returnerar en sträng där alla förekomster av ett reguljärt uttryck ersätts med en annan sträng
replaceRegex :: String -> String -> String -> String
replaceRegex old new = Text.Regex.replace (Text.Regex.mkRegex old) new

-- Returnerar en sökbar sträng som kan användas för att hitta alla förekomster av ett reguljärt uttryck
findRegex :: String -> String -> String
findRegex regex = Text.Regex.subRegex (Text.Regex.mkRegex regex)
```

Låt oss använda dessa funktioner på en exempelsträng:

```Haskell
let str = "Detta är en sträng som innehåller många ord"

-- Ersatt alla mellanslag med bindestreck
replace ' ' '-' str
>> "Detta-är-en-sträng-som-innehåller-många-ord"

-- Byt ut alla "o" för "å"
replaceAll "o" "å" str
>> "Dettå är en sträng som innehåller mångå ård"

-- Byt ut alla siffror med "X"
replaceRegex "[0-9]" "X" str
>> "Detta är en sträng som innehåller många ord"

-- Hitta alla ord som börjar med "m"
findRegex "\\bm\\w*" str
>> "många ord"
```

Som du kan se är det bara att använda rätt funktion beroende på ditt behov. Det kan vara bra att kolla igenom andra inbyggda funktioner och fundera på hur de kan användas för att söka och byta ut text.

##Djupdykning

En sak att komma ihåg när man söker och ersätter text i Haskell är att det finns flera olika sätt att göra det på. Till exempel kan du använda stringfunktioner som map och filter eller regex-baserade funktioner som vi visade ovan. Det är viktigt att välja rätt metod beroende på mängden data och hur komplex sökningen och ersättningen är för att få bästa möjliga prestanda.

En annan viktig aspekt att tänka på är att söka och ersätta strängar i Haskell generellt bara fungerar för ASCII-kodade tecken, inte Unicode. Det finns dock bibliotek som gör det möjligt att hantera Unicode-strängar, såsom utf8-string, för dem som behöver arbeta med flerspråkig text.

##Se även

Här är några användbara länkar för att lära dig mer om att söka och ersätta text i Haskell:

- [Haskell Dokumentation](https://www.haskell.org/documentation/)
- [Haskell strängfunktioner](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html)
- [Haskell regex-baserade funktioner](https://www.stackage.org/package/regex-base)
- [Haskell Unicode-bibliotek](https://hackage.haskell.org/package/utf8-string)