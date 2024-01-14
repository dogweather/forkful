---
title:                "Swift: Att läsa in matning från kommandoraden"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

#Varför
Att läsa in kommandoradsargument är en viktig del av att bli en effektiv Swift programmerare. Genom att kunna hantera externa inmatningar kan du skriva program som är mer flexibla och användbara för dina användare.

#Så här gör du
För att läsa kommandoradsargument i Swift, behöver du använda `CommandLine.arguments` som är en inbyggd array som innehåller alla inmatade argument från kommandoraden. Du kan använda `for-loop` för att iterera igenom arguments och sedan använda dessa värden i ditt program.

```Swift
for argument in CommandLine.arguments{
    print(argument)
}
```
När du kör detta program och ger det argument från kommandoraden, kommer den att skriva ut varje argument på en egen rad.

Om du vill hämta ett specifikt argument från listan kan du använda indexering. `CommandLine.arguments[0]` skulle till exempel returnera det första argumentet som matats in.

#Djupdykning
För att förstå mer om hur kommandoradsargument fungerar i Swift, är det viktigt att förstå att de är av typen `String`. Det betyder att du måste konvertera dem till andra datatyper om du vill använda dem som sådana.

Du kan också använda ett tredjepartsbibliotek, såsom `CommandLineKit`, för att göra inläsningen av argumenten enklare och mer robust. Detta bibliotek tillåter dig till exempel att definiera vilken typ av data ett argument ska vara (som till exempel `Int` eller `Bool`).

#Se även
- [Dokumentation om CommandLine i Swift](https://developer.apple.com/documentation/swift/commandline)
- [CommandLineKit på GitHub](https://github.com/jatoben/CommandLine)
- [Tutorial på reading command line arguments i Swift](https://medium.com/xcblog/command-line-arguments-in-swift-c9868fd85402)