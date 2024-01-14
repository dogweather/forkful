---
title:    "Swift: Radera tecken som matchar ett mönster"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara användbart för att filtrera eller rensa data som inte är önskvärt i en Swift applikation.

## Så här gör du

Först måste vi använda en ```NSRegularExpression``` för att definiera mönstret vi vill matcha. Sedan kan vi använda ```stringByReplacingMatches(in:options:range:withTemplate:)``` funktionen för att ta bort de matchande tecknen från en sträng.

```Swift
let regex = try! NSRegularExpression(pattern: "[0-9]", options: [])
let str = "En 2katt 1åt 3fiskar"
let range = NSRange(location: 0, length: str.utf16.count)
let result = regex.stringByReplacingMatches(in: str, options: [], range: range, withTemplate: "")
print(result) // Output: "En katt åt fiskar"
```

## Djupdykning

Utöver att ta bort vissa tecken kan vi också använda ```replacementString(for:)``` funktionen för att byta ut matchande tecken mot specifika strängar. Till exempel kan vi byta ut alla bokstäver mot stjärnor.

```Swift
let regex = try! NSRegularExpression(pattern: "[a-z]", options: [])
let str = "Ett äpple om dagen håller doktorn borta"
let range = NSRange(location: 0, length: str.utf16.count)
let result = regex.stringByReplacingMatches(in: str, options: [], range: range, withTemplate: "*")
print(result) // Output: "*** ****** ** ****** ******* ******"
```

## Se även

- [Apple's NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [NSHipster NSRegularExpression Article](https://nshipster.com/nsregularexpression/)