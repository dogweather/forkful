---
date: 2024-01-20 17:39:29.247998-07:00
description: "Converting a string to lowercase means swapping any uppercase letters\
  \ to their lowercase counterparts. Programmers do this for consistency, often for\u2026"
lastmod: '2024-03-13T22:45:00.384264-06:00'
model: gpt-4-1106-preview
summary: Converting a string to lowercase means swapping any uppercase letters to
  their lowercase counterparts.
title: Converting a string to lower case
weight: 4
---

## How to:
Swift makes this easy with a property called `lowercased`. Here's how you use it:

```Swift
let originalString = "Hello, World!"
let lowercasedString = originalString.lowercased()
print(lowercasedString) // "hello, world!"
```

Sample output:
```
hello, world!
```

## Deep Dive:
Historically, ensuring consistent string case has been crucial in programming, mainly since early computers were very case-sensitive. In Swift, `lowercased()` is a method available on instances of the `String` type. By invoking it, you convert all characters within the string that have lowercased variants to their lowercased forms.

Alternatives to `lowercased()` could be manually traversing the string and replacing each character with its lowercased equivalent by using a mapping function. But, honestly, that's reinventing the wheel.

String lowercasing has some nuances. For instance, the `lowercased()` method uses the current locale to handle specific language casing rules, which is not always the desired behavior. If you need to perform locale-independent conversions, you can resort to `lowercased(with: Locale?)` and pass `nil` as the Locale:

```Swift
let turkishString = "İstanbul"
let lowercasedTurkishString = turkishString.lowercased(with: nil)
print(lowercasedTurkishString) // "i̇stanbul", correct in Unicode, but 'I' without dot might be expected in Turkey.
```

The implementation of `lowercased()` under the hood leverages the Unicode standard which includes complex mapping rules for characters in various scripts, not all of which are a simple matter of 'a' replacing 'A'.

## See Also:
To explore more on strings and character transformations in Swift, dip into the following resources:

- Swift String and Characters documentation: [Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Unicode case mapping details: [Unicode Standard](https://www.unicode.org/reports/tr21/tr21-5.html)
- A discussion on string comparison and locale: [NSHipster Article on Locale](https://nshipster.com/locale/)
