---
title:                "Haskell: Sammanslående av strängar"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Varför

Att sammanfoga strängar (också känd som "konkatenation") är en viktig funktion inom programmering. Det gör det möjligt för oss att kombinera flera mindre strängar till en enda sträng, vilket kan vara till nytta för att skapa dynamiska meddelanden, filnamn eller evenemangstitlar.

## Hur man gör det

I Haskell, kan vi enkelt sammanfoga strängar med hjälp av "+" operatorn. Låt oss ta en titt på ett exempel:

```Haskell
strang1 = "Hej"
strang2 = "världen!"

sammanfogad = strang1 + strang2

print sammanfogad
```

Output: Hej världen!

Som du kan se, har vi använt "+" operatorn för att slå ihop två strängar, "Hej" och "världen!", och resultatet är nu en enda sträng, "Hej världen!"

Det är också möjligt att sammanfoga flera strängar på en gång, till exempel:

```Haskell
strang1 = "Det är"
strang2 = "en"
strang3 = "vacker"
strang4 = "dag"

sammanfogad = strang1 + strang2 + strang3 + strang4

print sammanfogad
```

Output: Det är en vacker dag.

## Djupdykning

Även om operatören "+" är den vanligaste metoden för att sammanfoga strängar i Haskell, finns det även andra metoder som kan vara användbara beroende på ditt användningsfall.

En alternativ metod är att använda "++" operatorn, vilket också sammanfogar två strängar, men skapar en ny lista istället för en sträng. Detta kan vara användbart om du vill hantera enskilda tecken i dina strängar.

En annan användbar funktion är "concat" som sammanfogar en lista av strängar till en enda sträng. Detta kan vara användbart om du har en variabel mängd strängar som du behöver sammanfoga.

## Se även

- [Haskell dokumentation om strängar](https://www.haskell.org/documentation/#strings)
- [En guide till Haskell för nybörjare](https://www.tutorialspoint.com/haskell/)