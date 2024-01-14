---
title:                "Swift: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
I den här bloggposten kommer vi att titta på hur du kan använda reguljära uttryck (regular expressions på engelska) i Swift. Reguljära uttryck är ett kraftfullt verktyg för att söka, matcha och manipulera textsträngar. Med hjälp av reguljära uttryck kan du enkelt hitta och extrahera specifika delar av en textsträng eller filtrera bort oönskad information. Det är ett ovärderligt verktyg för att hantera textbaserade data och kan användas i många olika sammanhang, som till exempel att validera inmatningsfält i ett formulär eller filtrera e-postmeddelanden.

## Så här gör du
För att använda reguljära uttryck i Swift behöver du importera Foundation framework. Det innehåller klassen NSRegularExpression som ger oss alla verktyg vi behöver för att skapa, validera och manipulera reguljära uttryck. Låt oss titta på ett exempel där vi vill hitta alla e-postadresser i en textsträng:

```Swift
import Foundation

let text = "Kontakta mig på john.doe@gmail.com eller john@doe.com"
let pattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
let regex = try! NSRegularExpression(pattern: pattern, options: .caseInsensitive)
let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))

for match in matches {
  let emailRange = Range(match.range, in: text)!
  let email = text[emailRange]
  print(email)
}
```

I det här exemplet skapar vi ett reguljärt uttryck som matchar en standard e-postformat. Vi använder sedan detta uttryck för att söka igenom vår textsträng efter matchande e-postadresser. Genom att använda rangen från matchobjektet kan vi få ut e-postadresserna och skriva ut dem.

## Djupdykning
Reguljära uttryck är uppbyggda av speciella tecken och teckenkombinationer som gör det möjligt att uttrycka mönster för att söka igenom en textsträng. Här är några av de vanligaste tecknen:

- `.` Matchar ett vilket tecken som helst.
- `*` Matchar 0 eller flera förekomster av föregående tecken.
- `+` Matchar 1 eller flera förekomster av föregående tecken.
- `?` Matchar 0 eller 1 förekomst av föregående tecken.
- `[]` Definierar en mängd av tecken som ska matchas.
- `()` Definierar en grupp av tecken som ska matchas.

Det finns många fler tecken och kombinationer som kan användas, men dessa är några av de mest användbara. Det kan ta lite tid att förstå hur man bygger ett effektivt reguljärt uttryck, men när du väl har förstått grundläggande koncept så finns det inga gränser för vad du kan åstadkomma.

## Se även
- [Swift String Dokumentation](https://developer.apple.com/documentation/swift/string)
- [Foundation Framework Dokumentation](https://developer.apple.com/documentation/foundation)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html) (engelska)