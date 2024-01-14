---
title:    "Swift: Läsning av kommandoradsargument"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför?

Om du är en utvecklare eller programmerare som jobbar med Swift, kan du ha stött på behovet av att läsa in kommandoradsargument. Det är ett vanligt krav inom olika projekt, särskilt när det gäller att göra din app anpassningsbar för användare. Att lära sig hur man läser in kommandoradsargument kan ge dig en ökad flexibilitet och bättre användarupplevelse.

## Så här gör du:

För att läsa in kommandoradsargument i Swift är stegen ganska enkla. Du behöver bara följa några enkla kodexempel för att komma igång.

````Swift
import Foundation

let arguments = CommandLine.arguments // läser in argumenten från kommandoraden

print(arguments) // skriver ut alla argumenten som en array
````

Om du till exempel kör detta program från kommandoraden:
```` 
$ swift hello.swift argument1 argument2
````
så kommer du att få utskriften: 
```` 
["hello.swift", "argument1", "argument2"]
````

Som du kan se är kommandoradsargumenten lästa in och lagrade i en array, där det första argumentet i arrayen är namnet på Swift-filen som körs. För att komma åt de andra argumenten behöver du bara använda index i arrayen (t.ex. `arguments[1]` för `argument1`).

## Djupdykning:

Nu när du vet hur man läser in kommandoradsargument i Swift, låt oss undersöka lite djupare och titta på några andra sätt att hantera dem.

En användbar funktion är att kunna kontrollera antalet inmatade argument. Du kan använda `arguments.count` för att få ut antalet argument och sedan använda villkorsinstruktioner för att göra olika saker baserat på antalet argument. Till exempel:

```` Swift
if arguments.count >= 3 { // om antalet argument är 3 eller fler
  // gör något 
}
else if arguments.count == 2 { // om antalet argument är 2
  // gör något annat
}
else { // om antalet argument är mindre än 2
  // gör något tredje
}
````

En annan möjlighet är att använda `CommandLine.arguments.joined(separator: " ")` för att slå samman alla argument till en enda sträng.

Det finns också en rad andra metoder som kan hjälpa dig att hantera och manipulera kommandoradsargument. Utforska gärna dem för att hitta de som är mest användbara för dina behov!

# Se också:

- [Swift Documentation](https://developer.apple.com/documentation/swift/command-line-argument-handling)
- [Stack Overflow: How do I get command line arguments in Swift?](https://stackoverflow.com/questions/24002864/how-do-i-get-the-command-line-arguments-in-swift/38002185#38002185) (på engelska)
- [Ray Wenderlich: Command Line Programs in Swift](https://www.raywenderlich.com/8113416-command-line-programs-in-swift) (på engelska)

Tack för att du läste och lycka till med att läsa in kommandoradsargument i Swift!