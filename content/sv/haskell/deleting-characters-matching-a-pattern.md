---
title:                "Haskell: Radera tecken som matchar ett mönster"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Att ta bort tecken som matchar ett visst mönster är en vanlig uppgift inom programmering, särskilt inom Haskell. Genom att förstå hur man kan ta bort dessa tecken kan du effektivisera din kod och göra den mer läsbar och kompakt.

## Så här gör du
Att ta bort tecken som matchar ett visst mönster i Haskell är en relativt enkel process, tack vare den kraftfulla "filter" funktionen. Först måste vi definiera en lista med tecken vi vill ta bort från en given sträng.

```Haskell
pattern :: [Char]
pattern = "aeiou"
```

I detta exempel har vi definierat en lista med vokaler som vi vill ta bort från en sträng. Nästa steg är att skapa en funktion som tar emot en sträng och använder "filter" funktionen för att ta bort de önskade tecknen.

```Haskell
removePattern :: String -> String
removePattern str = filter (\x -> not (elem x pattern)) str
```

I denna funktion använder vi "filter" tillsammans med en anonym funktion som kollar om varje tecken i strängen finns i vår definierade lista "pattern". Om tecknet finns i listan tas det bort från strängen, annars behålls det. Låt oss testa funktionen med en enkel sträng:

```Haskell
removePattern "Hello world!"
```

Output:

```
Hll wrld!
```

Som vi ser har alla vokaler tagits bort från strängen enligt vårt definierade mönster. Detta är en enkel och effektiv metod för att ta bort tecken som matchar ett visst mönster i Haskell.

## Djupdykning
Ett annat sätt att ta bort tecken som matchar ett visst mönster är att använda funktionen "delete" från "Data.List" modulen. Denna funktion tar emot ett element och en lista och tar bort alla förekomster av det givna elementet från listan.

```Haskell
removePattern' :: String -> String
removePattern' str = foldl (flip delete) str pattern
```

Här använder vi "foldl" funktionen tillsammans med "flip delete" för att ta bort varje tecken från strängen som matchar vårt definierade mönster. Denna metod kan vara lite mer avancerad än den föregående, men det är alltid bra att ha flera alternativ att använda i din kod.

## Se också
- [List Comprehensions in Haskell](https://wiki.haskell.org/List_comprehension)
- [Haskell Documentation](https://downloads.haskell.org/~ghc/8.8.1/docs/html/libraries/)
- [Real World Haskell](http://book.realworldhaskell.org/)