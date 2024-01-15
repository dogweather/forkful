---
title:                "Utskrift av felsökningsresultat"
html_title:           "Swift: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debugoutput är ofta ett bra sätt att felsöka och förstå vad som händer i koden. Det kan hjälpa dig att förstå hur olika delar av din kod interagerar och hitta eventuella fel.

## Så här gör du

För att skriva ut debugoutput i Swift, använder du funktionen ```print()```. Det är enkelt att använda och du kan skriva ut olika typer av värden som strängar, nummer och booleska värden.

```Swift
let name = "Anna"
let age = 25
let isStudent = true

print("Namnet är \(name), åldern är \(age) och personen är en student: \(isStudent)")
// Output: Namnet är Anna, åldern är 25 och personen är en student: true
```

Du kan också skriva ut värden från ett objekt genom att använda punktnotation, som i exemplet nedan.

```Swift
struct Person {
    var name: String
    var age: Int
    var isStudent: Bool
}

let person = Person(name: "Anna", age: 27, isStudent: true)

print("Namnet är \(person.name), åldern är \(person.age) och personen är en student: \(person.isStudent)")
// Output: Namnet är Anna, åldern är 27 och personen är en student: true
```

En annan användbar funktion är ```dump()```, som visar mer detaljerad information om ett objekt, inklusive dess egenskaper och värden.

```Swift
let books = ["Pride and Prejudice", "Jane Eyre", "War and Peace"]

dump(books)
// Output:
// ▿ 3 elements
// - 0: "Pride and Prejudice"
// - 1: "Jane Eyre"
// - 2: "War and Peace"
```

## Djupdykning

Att skriva ut debugoutput kan vara ett kraftfullt verktyg för att förstå din kod och hitta fel. Det är också ett användbart sätt att kommunicera med andra utvecklare och dela information om din kod.

En viktig sak att komma ihåg är att ta bort alla utskrifter av debugoutput när du är färdig med din felsökning. Annars kan det leda till prestandaproblem och onödig datautskrift i produktion.

Det finns också andra sätt att skriva ut debugoutput, som att använda en debugger eller loggningsramverk som Log4j. Det är viktigt att undersöka vilka alternativ som är bäst för din specifika kod och arbetsmiljö.

## Se även

- [Debugging in Swift](https://www.raywenderlich.com/10452924-debugging-in-swift)
- [Printing Debug Information in Swift](https://medium.com/better-programming/printing-debug-information-in-swift-73b65b9da01d)
- [Debugging with Xcode's LLDB Debugger](https://www.hackingwithswift.com/read/6/3/debugging-with-xcodes-lldb-debugger)