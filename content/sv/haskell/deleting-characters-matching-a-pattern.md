---
title:    "Haskell: Raderar tecken som matchar ett mönster"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

I Haskell-programmering kan det ibland vara nödvändigt att ta bort vissa tecken som matchar ett visst mönster. Detta kan göras av flera olika anledningar, såsom att rensa bort onödiga tecken i en sträng eller matcha användarinput mot ett specifikt mönster.

## Hur man gör

För att ta bort karaktärer som matchar ett mönster i Haskell, kan vi använda funktionen `deleteBy`. Denna funktion tar emot ett predikat och en lista och returnerar en ny lista utan element som matchar predikatet.

```Haskell
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]

deleteBy (==) 'a' "Haskell" -- Output: "Hskell"
deleteBy (>=) 3 [1,2,3,4,5] -- Output: [1,2]
```

I det första exemplet använder vi funktionen för att ta bort alla förekomster av bokstaven 'a' i strängen "Haskell". I det andra exemplet tar vi bort alla tal som är större än eller lika med 3 i listan [1,2,3,4,5].

## Djupdykning

`deleteBy` är en allmän funktion som tar emot ett predikat som jämför två element. Detta innebär att vi kan använda den för att lösa många olika problem som involverar att ta bort element från en lista baserat på ett visst kriterium.

För att se en detaljerad implementering av `deleteBy`, kan du kolla på [Haskells dokumentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Data.OldList.html#deleteBy). Det finns också flera andra funktioner inom Haskell som kan användas för att ta bort element från listor, såsom `filter` och `delete`.

## Se även

- [Haskell-funktionen `deleteBy` på Haskells dokumentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Data.OldList.html#deleteBy)
- [Haskells dokumentation om listor och listfunktioner](https://www.haskell.org/onlinereport/standard-prelude.html#list-functions) 
- [En tutorial om grundläggande funktionell programmering i Haskell](https://medium.com/@erinbarker/functional-programming-in-haskell-443f3914f56)