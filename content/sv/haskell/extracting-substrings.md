---
title:    "Haskell: Extrahering av delsträngar"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

I många programmeringsspråk finns det inbyggda funktioner för att extrahera delsträngar från en huvudsträng. Men varför skulle man vilja göra det? Det finns flera bra anledningar:

- Skapa en underserie av data som endast innehåller relevanta delar
- Manipulera data och sedan återansluta det till den ursprungliga strängen

Haskell har också funktioner för att extrahera delsträngar, och i den här bloggposten ska vi utforska hur man kan använda dem.

## Så här gör du

För att börja med behöver vi en sträng som vi vill extrahera delar av. Låt oss säga att vi har en sträng som representerar en e-postadress:

```
myString = "example@blogpost.com"
```

Vi kan nu använda funktionen `take` för att hämta en del av strängen. Den tar två argument - antalet tecken vi vill hämta och själva strängen. Vi kan till exempel hämta bara användarnamnet från e-postadressen:

```
take 7 myString
```

Output:
```
"example"
```

Om vi istället vill hämta domännamnet kan vi använda funktionen `drop`, som tar samma argument men hämtar det som ligger kvar efter de antal tecken vi angivit:

```
drop 8 myString
```

Output:
```
"blogpost.com"
```

Vi kan också kombinera dessa två funktioner för att extrahera en del av en sträng. Låt oss säga att vi endast är intresserade av domänändelsen:

```
drop 14 (take 17 myString)
```

Output:
```
".com"
```

## Djupdykning

För att kunna extrahera delsträngar behöver man förstå hur man manipulerar listor i Haskell. Både `take` och `drop` fungerar på listor, och det är därför de används för att hämta delar av strängar. För att lära dig mer om listmanipulation i Haskell kan du kolla in dessa resurser:

- [Haskell.org Tutorial: Lists](https://wiki.haskell.org/Lists)
- [Learn You a Haskell: Starting Out - Lists](http://learnyouahaskell.com/starting-out#an-intro-to-lists)
- [Real World Haskell: Lists and tuples](http://book.realworldhaskell.org/read/functional-programming.html#functional-lists)

## Se även

- [Haskell.org: Text manipulation functions](https://www.haskell.org/hoogle/?hoogle=text+manipulation)
- [Haskell.org Tutorial: Strings](https://wiki.haskell.org/Strings)
- [Haskell Wikibook: String processing](https://en.wikibooks.org/wiki/Haskell/String_processing)