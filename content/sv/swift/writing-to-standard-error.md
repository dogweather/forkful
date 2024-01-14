---
title:    "Swift: Skriva till standard error"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Det finns många olika sätt att skriva ut information i Swift-programmering, men att skriva till standard error är särskilt användbart för felhantering och debugging. Genom att skriva till standard error får du en separat kanal för att skriva ut meddelanden som inte blandas med programkörningen eller normal utdata. Detta kan vara till stor hjälp när du letar efter fel och behöver spåra exakt var det uppstod.

## Så här gör du

För att skriva till standard error i Swift, behöver du först importera Foundation framework. Sedan kan du använda funktionen "print" tillsammans med parametern "to:" för att specifiera att du vill skriva till standard error istället för standard utdata.

```Swift
import Foundation

print("Detta är ett meddelande till standard error", to: &.standardError))
```

Detta kommer att skriva ut meddelandet till standard error-kanalen och det kan enkelt skiljas från andra utskrifter genom att det följer "skalet" taggen. Om du till exempel kör detta i en terminal, kommer resultatet att se ut så här: 

```bash
$ ./program
Detta är ett meddelande till standard error
$
```

Som du kan se är meddelandet isolerat och lätt att hitta i mitten av skalet taggen. Detta är särskilt användbart när du kör stora och komplexa program med många utskrifter.

## Djupdykning

Standard error anses vara en av tre strömmar för att hantera informationsflöden i Swift-programmering, tillsammans med standard utdata (standard output) och standard indata (standard input). Det är en del av POSIX-standarderna och är också standarden för de flesta Unix-system. Det är en viktig del av felhantering och debugging processen och används ofta tillsammans med andra verktyg som till exempel Felsökaren (Debugger) och loggfiler.

Du kan också använda standard error tillsammans med andra Swift-konstruktioner för att skriva ut mer information till felmeddelanden, som till exempel Stack Traces.

## Se även

- [Swift API dokumentation för print() funktionen](https://developer.apple.com/documentation/swift/1541053-print)
- [Techotopia: Swift - Swift Error Handling](https://www.techotopia.com/index.php/Outputting_Information_to_the_Console_in_Swift#Swift_Error_Handling) 
- [Ray Wenderlich: How to Work with File System in Swift](https://www.raywenderlich.com/661-foundation-framework-tutorial-how-to-work-with-file-systems-in-swift)