---
title:                "C++: Radera tecken som matchar ett mönster"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att radera tecken som matchar ett mönster kan vara användbart i många olika situationer. Det kan till exempel vara för att rensa bort oönskad eller felaktig data från en sträng eller för att förbereda data för bearbetning eller analys.

## Så här gör du

För att radera tecken som matchar ett visst mönster, behöver vi använda oss av några av de inbyggda funktionerna i C++. Nedan följer ett kodexempel:

```C++
// Definiera en sträng med texten vi vill modifiera
std::string text = "Detta är en sträng med några oönskade tecken.";

// Iterera igenom alla tecken i strängen
for (int i = 0; i < text.length(); i++) {

    // Kontrollera om tecknet matchar mönstret vi vill radera
    if (text[i] == 'ö' || text[i] == 'ö') {
        
        // Om tecknet matchar, radera det från strängen
        text.erase(i, 1);

        // Minska sedan indexvärdet för att korrekt hantera resten av strängen
        i--;
    }
}

// Skriv ut den nya strängen utan de borttagna tecknen
std::cout << text << std::endl;
```

Nedan är det förväntade resultatet av ovanstående kod:

```
Deta är en strng med ngra onskade tecken.
```

## Fördjupning

För att förstå koden ovan behöver vi först förstå de inbyggda C++ funktionerna som används. Den första funktionen är `length()`, vilken returnerar antalet tecken i en sträng. Därefter har vi funktionen `erase()`, som används för att ta bort en del av en sträng. Den tar två argument - för det första indexpositionen för den del av strängen som vi vill radera, och sedan antalet tecken vi vill radera.

Den andra delen av koden är en typisk `for`-loop som gör det möjligt för oss att gå igenom var och ett av tecknen i strängen. Men innan vi ökar indexvärdet måste vi minska det med 1 för att korrekt hantera resten av strängen efter att ett tecken har raderats.

## Se även

- [C++ string-funktioner](https://www.cplusplus.com/reference/string/string/)
- [C++ for-loop](https://www.cplusplus.com/doc/tutorial/control/#iteration-statements)