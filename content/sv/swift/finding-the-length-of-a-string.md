---
title:    "Swift: Att hitta längden på en sträng"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande och viktig funktion inom programmering. Genom att kunna bestämma längden på en sträng kan du göra olika operationer som manipulerar och hanterar text.

## Så här gör du

För att hitta längden på en sträng i Swift kan du använda funktionen `count`, som finns inbyggd i Swift's `String` typ. Här är ett enkelt exempel:

```Swift
let text = "Hej alla Läsare!"
print(text.count)
```

I detta exempel skapar vi en variabel `text` som innehåller en sträng av text. Sedan använder vi funktionen `count` för att bestämma längden på strängen och skriva ut resultatet i konsolen. I detta fall kommer resultatet att vara 16, eftersom det finns 16 tecken i strängen.

Om du vill hitta längden på en sträng som innehåller specialtecken eller olika språk, behöver du använda en annan metod. Då kan du använda `count` tillsammans med `utf16.count` för att få en korrekt längd på strängen.

```Swift
let text = "Hello 𝕃𝕒𝕤𝕖𝕣𝕤!"
print(text.count) // 14
print(text.utf16.count) // 20
```

Som du kan se i detta exempel, är det mest exakta sättet att bestämma längden på en sträng att använda `utf16.count`.

## Djupdykning

I Swift är `String` typen en samling av Unicode-tecken. Det betyder att det kan hantera tecken och symboler från olika språk och skript. Det är också viktigt att komma ihåg att inte alla tecken är lika långa. Till exempel så är ett ASCII-tecken (7-bitars-tecken) vanligtvis ett tecken, men ett icke-ASCII-tecken (8-bitars-tecken) kan vara en, två eller fyra bytes långt. Det är därför det är viktigt att använda `utf16.count` för att få den korrekta längden på en sträng.

Det finns också vissa tecken som inte kan representeras som enskilda Unicode-tecken. Dessa tecken kallas "sammansatta tecken" och kan ta upp flera positioner i en sträng. I Swift kan du använda `unicodeScalars` för att hitta längden på dessa sammansatta tecken.

## Se även

- [Officiell dokumentation för Swift's String typ](https://developer.apple.com/documentation/swift/string)
- [Unicode på Swift](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PDFKitGuide/PDFKit_ProgTopic/unicode.html)