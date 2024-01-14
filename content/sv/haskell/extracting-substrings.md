---
title:                "Haskell: Extrahera delsträngar"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I Haskell-programmering kommer du ofta stöta på behovet av att dela upp en sträng eller lista i mindre delar, kallade substrings. Detta kan vara till nytta när du behöver behandla data separat eller extrahera specifika delar för vidare bearbetning. Genom att lära dig att extrahera substrings kommer du kunna hantera data mer effektivt och skriva mer kompakt kod.

## Så här gör du

Följande exempel visar hur du lätt kan extrahera en substring från en sträng i Haskell:

```Haskell
-- Deklarera en sträng
let str = "Hello, world!"

-- Extrahera en substring med hjälp av "drop" och "take" funktionerna
let substring = take 5 (drop 7 str)

-- Skriv ut substrings
print substring
```

Resultatet av koden kommer att vara "world", vilket är de fem tecken som börjar på index 7 i strängen "Hello, world!". Som du kan se använder vi här de inbyggda "take" och "drop" funktionerna, som låter oss välja en del av strängen utifrån dess index. Genom att kombinera dessa funktioner kan du precisera vilka tecken du vill extrahera och på vilken position de befinner sig.

Du kan också använda "splitAt" funktionen för att dela upp en sträng i två delar vid ett givet index. Exempelvis kan vi dela upp strängen "Hello, world!" i "Hello," och "world!" genom att använda funktionen på index 5:

```Haskell
-- Deklarera en sträng
let str = "Hello, world!"

-- Dela strängen vid index 5
let (first, second) = splitAt 6 str

-- Skriv ut resultaten
print first
print second
```

Resultatet blir "Hello," för "first" och "world!" för "second". På detta sätt kan du dela upp strängen i precis de delar som du behöver, beroende på vilken behandling du vill göra.

En annan användbar funktion för att extrahera substrings är "subsequences". Denna funktion returnerar alla möjliga delar av en sträng, vilket kan vara användbart i vissa applikationer.

## Djupdykning

När du extraherar substrings i Haskell är det viktigt att tänka på att alla inbyggda funktioner för strängmanipulering returnerar en ny sträng istället för att ändra den befintliga strängen. Detta gör att du behåller originalsträngen för vidare användning, samtidigt som du kan skapa nya delar av den. Dessutom är funktionerna "take" och "drop" överlagrade för olika datatyper, så du kan använda dem på både strängar och listor.

Det finns också andra bibliotek och paket som erbjuder olika funktioner för att extrahera substrings mer specifikt, beroende på dina behov. Det kan vara värt att undersöka och utforska dessa alternativ för att hitta den bästa metoden för din specifika användning.

## Se även

- [Haskells dokumentation för strängmanipulering](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-String.html)
- [Haskells dokumentation för listfunktioner](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html)
- [Mer information om substrings i Haskell](https://www.tutorialspoint.com/haskell/haskell_string_manipulation.htm)