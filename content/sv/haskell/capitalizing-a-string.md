---
title:                "Haskell: Stora versaler för en sträng"
simple_title:         "Stora versaler för en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en vanlig uppgift i programmering. Det kan vara användbart för att få ut en sträng med stora bokstäver, vilket ofta används i rubriker och andra viktiga delar av en applikation. I denna bloggpost kommer vi att utforska hur man skriver en funktion för att kapitalisera en sträng i Haskell.

## Hur man gör

För att kapitalisera en sträng i Haskell kan du använda en inbyggd funktion som heter `toUpper`. Den tar en enskild bokstav och returnerar den motsvarande versalen. Med hjälp av denna funktion kan vi skriva en funktion för att kapitalisera en hel sträng. Här är ett exempel på hur det skulle kunna se ut:

```
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs

main = do
  putStrLn "Skriv in en sträng: "
  str <- getLine
  putStrLn ("Kapitaliserad sträng: " ++ capitalize str)
```

I detta exempel definieras funktionen `capitalize` vilken tar emot en sträng (av typen `String`) som argument. I det första mönstermatchningsmönstret kontrolleras om den givna strängen är tom, i så fall returneras en tom sträng. I det andra mönstermatchningsmönstret tar vi första bokstaven av strängen och använder `toUpper` för att göra det till en versal. Sedan rekursivt anropas funktionen igen med resten av strängen (exklusive den första bokstaven) genom att använda rekursion (som förklaras mer detaljerat i sektionen "Deep Dive").

I huvudfunktionen `main` använder vi `getLine` för att få en input-sträng från användaren och sedan skriver ut den kapitaliserade strängen med hjälp av `putStrLn`.

Om vi kör detta program med en input-sträng som "hej, världen!" kommer output att vara "Hej, världen!".

## Deep Dive

Rekursion är en viktig del av många funktioner i Haskell. I vårt exempel gör vi rekursiva anrop till `capitalize` för att bearbeta varje bokstav i strängen. Det första anropet kapitaliserar första bokstaven, och sedan fortsätter anropen rekursivt med resten av strängen tills vi når en tom sträng.

Det finns också andra metoder för att kapitalisera en sträng i Haskell, som att använda funktionen `map` tillsammans med `toUpper`. Detta skulle se ut så här:

```
capitalize :: String -> String
capitalize str = map toUpper str
```

Med hjälp av `map` kan vi applicera `toUpper` på varje enskild bokstav i strängen och få tillbaka en kapitaliserad sträng.

## Se även

- [Haskell.org](https://www.haskell.org) - Officiell webbplats för Haskell och dess dokumentation.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - En populär onlinebok för att lära sig Haskell.
- [Haskell - En introduktion](https://www.programmica.com/2018/10/04/haskell-en-introduktion/) - En annan Haskell-bloggpost på vår sida (tillgänglig på svenska).