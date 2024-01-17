---
title:                "Utskrift av felsökningsutdata"
html_title:           "Swift: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & varför?
Debug-utmatning är en process där du som programutvecklare skriver ut information om vad som sker i ditt program under körning. Detta kan hjälpa dig att hitta och lösa fel i din kod.

## Hur man gör:
Här är ett exempel på hur du kan skriva ut debug-utmatning i Swift:

```Swift
// Skapa en variabel som innehåller ett meddelande
let message = "Välkomna till Swift-programmering!"

// Skriv ut meddelandet till konsolen
print(message)
```

Detta kommer att skriva ut "Välkomna till Swift-programmering!" i konsolen när du kör ditt program. Du kan även skriva ut värden från variabler, till exempel:

```Swift
let num1 = 2
let num2 = 9

print("Summan av \(num1) och \(num2) är \(num1 + num2)")
```

Detta kodexempel kommer att skriva ut "Summan av 2 och 9 är 11" i konsolen.

## Djupdykning:
Debug-utmatning har varit en viktig del av programmering sedan början av datorernas tid. Innan det fanns graphiska gränssnitt var debug-utmatning det enda sättet att hålla koll på vad som hände i programmen.

En alternativ metod för debug-utmatning är användandet av s.k. breakpoints, där programmet pausas när en viss kodrad har nåtts. Detta kan vara ett effektivare sätt att hitta buggar i vissa situationer.

I Swift kan du också använda funktionen "assert()" för att skriva ut felmeddelanden om ett visst villkor inte uppfylls.

## Se även:
[Apple Documentation on Debugging](https://developer.apple.com/documentation/xcode/debugging)