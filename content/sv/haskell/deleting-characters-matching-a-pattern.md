---
title:    "Haskell: Radera tecken som matchar ett mönster"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett visst mönster kan vara användbart i många olika situationer. Det kan hjälpa till att rensa data, göra textbearbetning eller filtrera information.

## Hur man gör

För att ta bort tecken som matchar ett visst mönster i Haskell, kan vi använda funktionen `deleteBy`. Den tar emot två parametrar - ett predikat som definierar vilka tecken som ska tas bort och en lista som innehåller tecken som ska behandlas. I följande exempel vill vi ta bort alla små bokstäver från en textsträng:

```Haskell
deleteBy (\x -> x `elem` ['a'..'z']) "Detta är en text" 
```

Det första argumentet i funktionen är en anonym funktion som jämför varje tecken med listan som innehåller alla små bokstäver. Om tecknet finns i listan så tas det bort från strängen. Det resulterande värdet blir "Detta ".

Vi kan också använda en predikatfunktion som är definierad på ett annat sätt. Om vi vill ta bort alla siffror från en textsträng, kan vi skriva följande:

```Haskell
deleteBy isDigit "123 abc" 
```

I det här fallet är funktionen `isDigit` från standardbiblioteket `Char` som returnerar `True` om ett tecken är en siffra. Det resulterande värdet blir " abc".

## Djupdykning

Funktionen `deleteBy` definieras på följande sätt:

```Haskell
deleteBy :: (a -> a -> Bool) -> [a] -> [a]
```

Det första argumentet är en jämförelsefunktion som tar emot två värden av typen `a` och returnerar en boolskt värde. Om funktionen returnerar `True` så tas det andra värdet bort från listan. Det andra argumentet är listan som ska behandlas.

En viktig aspekt att notera är att `deleteBy` inte har någon garanti för att bibehålla ordningen på listan. Det betyder att resultatet inte nödvändigtvis blir detsamma varje gång funktionen anropas.

## Se även

- [Haskell's deleteBy function](https://www.haskell.org/hoogle/?hoogle=deleteBy)
- [Haskell String Manipulation](https://wiki.haskell.org/String_manipulation)
- [Haskell List Functions](http://zvon.org/other/haskell/Outputprelude/index.html)