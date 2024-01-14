---
title:                "Swift: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Det finns många olika anledningar till varför man kan vilja använda sig av att skriva ut debug-information när man programmerar i Swift. Det kan hjälpa till att förstå hur koden körs och var eventuella fel uppstår, vilket underlättar vid felsökning och utveckling av nya funktioner.

## Så här gör du
För att skriva ut debug-information i Swift kan du använda dig av funktionen `print()`. Här är ett enkelt exempel på hur man kan använda den:

```Swift
let name = "Johan"
let age = 25
print("Hej, jag heter \(name) och jag är \(age) år gammal!")
```

Detta kommer att skriva ut följande i konsolen:

`Hej, jag heter Johan och jag är 25 år gammal!`

Du kan också skriva ut värdet av en variabel eller konstant direkt, utan att använda `print()` funktionen:

```Swift
let number = 5
print(number) // kommer att skriva ut 5 i konsolen
```

Du kan även skriva ut flera värden på en rad genom att separera dem med kommatecken inuti funktionen:

```Swift
let x = 2
let y = 4
let z = 6
print(x, y, z) // kommer att skriva ut "2, 4, 6" i konsolen
```

## Djupdykning
Det finns flera olika sätt att använda `print()` funktionen i Swift för att skriva ut debug-information. Du kan till exempel använda olika formateringsalternativ för att skriva ut värden på olika sätt, som decimaltal eller meddelanden vid fel. Du kan också använda dig av `debugPrint()` funktionen för att skriva ut mer detaljerad information om objekt och klasser.

Det är viktigt att använda sig av debug-output på rätt sätt och bara i utvecklingssyfte. Det är inte en bra idé att ha utskrifterna kvar i produktionskoden, eftersom det kan påverka prestandan och resurserna för din applikation.

## Se även
- [Apple Swift Language Guide on Debugging](https://docs.swift.org/swift-book/LanguageGuide/Debugging.html)
- [Printing Debug Output in Swift Playgrounds](https://developer.apple.com/videos/play/wwdc2018/407/)
- [Using print() for Swift debugging](https://www.hackingwithswift.com/example-code/language/using-print-for-swift-debugging)