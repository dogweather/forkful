---
title:    "Swift: Utvinna delsträngar"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I Swift programmering kan man ofta behöva extrahera delar av en textsträng för att utföra olika operationer. Det kan vara allt från att ändra formatet på en textsträng till att söka efter specifika ord. Men varför är det nödvändigt att extrahera substrängar? Det kan bero på olika anledningar, som att göra det lättare att hantera data eller att förenkla vissa algoritmer.

## Så här gör du

För att extrahera en substräng i Swift använder man metoden `substring`. Den accepterar två parametrar - det första är startindex för substrängen och det andra är längden på substrängen. Här är ett exempel på hur man extraherar en substräng från en befintlig textsträng:

```Swift 
let text = "Hej världen!"
let startIndex = text.index(text.startIndex, offsetBy: 4)
let endIndex = text.index(text.endIndex, offsetBy: -1)
let substring = text.substring(with: startIndex ..< endIndex)
```
Detta kommer att resultera i att variabeln `substring` innehåller den nya textsträngen "världen". Notera att `offsetBy` används för att specificera startindexet och slutindexet för substrängen.

## Djupdykning

Nu när vi har sett hur man extraherar en substräng, låt oss gå in lite djupare på ämnet. Först och främst är det viktigt att förstå att index i Swift inte alltid matchar teckenpositionerna i en textsträng. Detta beror på att Swift använder Unicode och vissa tecken kan ha flera representationer. Därför är det viktigt att använda Swifts inbyggda funktioner för att räkna ut korrekta indexvärden när man extraherar substrängar.

En annan viktig sak att komma ihåg är att substrängar delar referens med den ursprungliga textsträngen. Detta innebär att förändringar i substrängen också påverkar den ursprungliga textsträngen och vice versa. Om du behöver en helt ny textsträng, använd istället `substring(with: Range<String.Index>)`.

## Se även

Här är några resurser som kan vara till hjälp när du arbetar med substrängar i Swift:

- [Officiell Swift dokumentation om substrängar](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID295) (på engelska)
- [En guide på svenska om Swift textsträngar och Unicode](https://habr.com/en/post/257099/) (engelsk översättning finns tillgänglig)
- [En tutorial på svenska om hur man extraherar och manipulerar substrängar i Swift](https://cocoacasts.com/how-to-work-with-strings-in-swift/)