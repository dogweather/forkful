---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:28.840971-07:00
description: "Regulj\xE4ra uttryck, eller regex, \xE4r sekvenser av tecken som bildar\
  \ ett s\xF6km\xF6nster, ofta anv\xE4nda f\xF6r str\xE4ngmatchning eller manipuleringsuppgifter.\u2026"
lastmod: 2024-02-19 22:04:57.480525
model: gpt-4-0125-preview
summary: "Regulj\xE4ra uttryck, eller regex, \xE4r sekvenser av tecken som bildar\
  \ ett s\xF6km\xF6nster, ofta anv\xE4nda f\xF6r str\xE4ngmatchning eller manipuleringsuppgifter.\u2026"
title: "Att anv\xE4nda regulj\xE4ra uttryck"
---

{{< edit_this_page >}}

## Vad och varför?
Reguljära uttryck, eller regex, är sekvenser av tecken som bildar ett sökmönster, ofta använda för strängmatchning eller manipuleringsuppgifter. Programmerare använder dem för allt från datavalidering och tolkning till transformationer, vilket gör dem till ett ovärderligt verktyg i textbehandling och manipuleringsuppgifter över olika programmeringsspråk, inklusive Swift.

## Hur:
Swifts inbyggda stöd för regex använder klassen `NSRegularExpression`, tillsammans med String-klassens metoderna för omfång och ersättning. Nedan följer ett exempel på hur regex kan användas för att hitta och markera e-postadresser inom en textblock:

```swift
import Foundation

let text = "Kontakta oss på support@example.com eller feedback@example.org för mer information."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Hittad: \(text[range])")
        }
    } else {
        print("Inga matchningar hittades.")
    }
} catch {
    print("Regexfel: \(error.localizedDescription)")
}

// Exempel på utmatning:
// Hittad: support@example.com
// Hittad: feedback@example.org
```

För mer komplexa eller bekvämlighetsfokuserade scenarion, kan du använda tredjepartsbibliotek såsom SwiftRegex, som förenklar syntaxen och utökar möjligheterna. Även om Swifts standardbibliotek är kraftfullt, föredrar vissa utvecklare dessa bibliotek för deras kortfattade syntax och extra funktioner. Så här kan du utföra en liknande uppgift med ett hypotetiskt tredjepartsbibliotek:

```swift
// Antag att ett bibliotek som heter SwiftRegex finns och är importerat
let text = "Kontakta oss på hello@world.com eller besök vår hemsida."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Hypotetisk metod som tillhandahålls av SwiftRegex
if emails.isEmpty {
    print("Inga e-postadresser hittades.")
} else {
    emails.forEach { email in
        print("Hittad: \(email)")
    }
}

// Hypotetisk utmatning antagandes att metoden `matches(for:)` finns i SwiftRegex:
// Hittad: hello@world.com
```

Detta exempel illustrerar användningen av ett tredjeparts reguljärt uttrycksbibliotek för att förenkla sökningen efter matchningar inom en sträng, under förutsättning att sådana bekvämlighetsmetoder som `matches(for:)` finns. Det är viktigt att hänvisa till den respektive tredjepartsbibliotekets dokumentation för korrekt syntax och metodtillgänglighet.
