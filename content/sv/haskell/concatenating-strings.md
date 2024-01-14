---
title:                "Haskell: Konkatenering av strängar"
simple_title:         "Konkatenering av strängar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har arbetat med textbaserade data, som användardata eller loggar, har du förmodligen stött på behovet av att kombinera eller sammanfoga flera strängar till en enda. I Haskell är det möjligt att göra detta genom en process som kallas förkättring. I denna bloggpost kommer vi att utforska detta koncept och se hur det kan användas i dina program.

## Så här gör du

För att sammanfoga strängar i Haskell använder vi operatorn `++`. Detta tar två strängar och sammanfogar dem till en enda sträng. Här är ett exempel på hur det kan se ut:

```Haskell
-- definiera två strängar
let str1 = "Hej "
let str2 = "världen!"

-- använda ++ operatorn för att sammanfoga strängarna
let str3 = str1 ++ str2

-- skriv ut resultatet
print(str3)
```

Output:

```Haskell
Hej världen!
```

Det är också möjligt att sammanfoga flera strängar på en gång genom att använda funktionsmetoden `concat`. Den tar en lista av strängar som argument och sammanfogar dem till en enda sträng. Här är ett exempel på hur det kan se ut:

```Haskell
-- definiera en lista av strängar
let strList = ["Haskell", "är", "kul", "!"]

-- använd concat för att sammanfoga strängarna i listan
let str = concat strList

-- skriv ut resultatet
print(str)
```

Output:

```Haskell
Haskell är kul!
```

## Djupdykning

Förkättring är en operation som kan utföras på alla typer av data som är instanser av klassen `Show`. Det innebär att inte bara strängar utan även andra typer av data kan sammanfogas. Detta gör det möjligt att skapa mer komplexa utskrifter eller loggar som kombinerar flera värden.

En annan användbar funktion för sammanfogning som är värd att nämna är `intercalate`. Detta tar en sträng och en lista av strängar som argument och sammanfogar strängarna i listan med den första strängen som ett mellanrum. Detta är särskilt användbart för att producera snyggare och lättläsliga utskrifter eller loggar.

## Se också

För mer information och exempel om hur man förkatar strängar, se följande länkar:

- [Haskell Strings](https://wiki.haskell.org/Strings)
- [Learn You a Haskell - Strings](http://learnyouahaskell.com/starting-out#strings)
  - [concat](http://learnyouahaskell.com/starting-out#string-functions)
  - [intercalate](http://learnyouahaskell.com/modules#intercalate)
- [Real World Haskell - Working with text](http://book.realworldhaskell.org/read/working-with-strings.html)