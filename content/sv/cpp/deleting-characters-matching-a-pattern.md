---
title:    "C++: Radera tecken som matchar ett mönster"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

I C++ programmering kan det ibland vara nödvändigt att ta bort vissa tecken som matchar ett visst mönster från en sträng. Det kan till exempel vara för att rensa bort onödiga mellanslag eller andra tecken som inte behövs i en specifik applikation. I den här bloggposten kommer vi att utforska olika sätt att ta bort karaktärer som matchar ett mönster och hur man kan använda denna funktion i sin kod.

## Hur man gör det

Det finns flera sätt att ta bort karaktärer från en sträng i C++. En av de enklaste metoderna är att använda en for-loop och en if-sats för att iterera genom strängen och ta bort de karaktärer som matchar det önskade mönstret.

```C++
// Skapa en sträng med en del onödiga mellanslag
std::string text = " Detta är en text med onödiga mellanslag. ";

// Loopa igenom strängen och ta bort mellanslagen
for (int i = 0; i < text.length(); i++) {
    if (text[i] == ' ') {
        text.erase(i, 1);
    }
}

// Skriv ut resultatet
std::cout << "Resultat: " << text << std::endl;
```

Koden ovan kommer att loopa igenom strängen och ta bort alla mellanslag genom att använda funktionen `erase()` och ange positionen för det tecken som ska tas bort. Efteråt skriver vi ut det nya resultatet som kommer att vara en sträng utan mellanslag.

Det finns också andra användbara funktioner för att ta bort karaktärer från en sträng, till exempel `remove_if()` och `regex_replace()`. Dessa funktioner kan vara användbara om man söker ett mer avancerat sätt att ta bort tecken från en sträng.

## Deep Dive

Funktionen `erase()` är en medlem av klassen `basic_string` i C++ standard biblioteket. Den tar emot två parametrar - första parametern är positionen för det tecken som ska tas bort och andra parametern är antalet tecken som ska tas bort. Detta tillåter oss att ta bort en enskild eller flera karaktärer från en sträng.

Om vi vill ta bort alla blanksteg från en sträng kan vi använda funktionen `remove_if()` som accepterar en predikatfunktion som ett argument. Denna funktion kommer att iterera genom strängen och ta bort alla tecken som uppfyller villkoret för predikatfunktionen, i vårt fall alla mellanslag.

```C++
// Skapa en sträng med blanksteg
std::string text = " Detta är en text med blanksteg. ";

// Använda funktionen remove_if för att ta bort blanksteg
text.erase(std::remove_if(text.begin(), text.end(), ::isspace), text.end());

// Skriv ut resultatet
std::cout << "Resultat: " << text << std::endl;
```

Slutligen, med hjälp av funktionen `regex_replace()` är det möjligt att ta bort tecken från en sträng genom att använda ett reguljärt uttryck som mönster. Detta ger oss en mer flexibel och kraftfull metod för att ta bort karaktärer från en sträng.

## Se även

- C++ standard biblioteket: http://www.cplusplus.com/reference/string/string/
- Regular Expressions i C++: http://www.cplusplus.com/reference/regex/
- Flera exempel på C++ kod: http://rosettacode.org/wiki/Category:C%2B%2B