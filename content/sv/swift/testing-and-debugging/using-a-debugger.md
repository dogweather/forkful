---
date: 2024-01-26 04:10:48.646727-07:00
description: "Hur: F\xF6r att anv\xE4nda fels\xF6karen i Xcode (den integrerade utvecklingsmilj\xF6\
  n f\xF6r Swift), kan du s\xE4tta brytpunkter, inspektera variabler och se \xF6ver\
  \ uttryck.\u2026"
lastmod: '2024-03-13T22:44:38.255620-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att anv\xE4nda fels\xF6karen i Xcode (den integrerade utvecklingsmilj\xF6\
  n f\xF6r Swift), kan du s\xE4tta brytpunkter, inspektera variabler och se \xF6ver\
  \ uttryck."
title: "Att anv\xE4nda en debugger"
weight: 35
---

## Hur:
För att använda felsökaren i Xcode (den integrerade utvecklingsmiljön för Swift), kan du sätta brytpunkter, inspektera variabler och se över uttryck. Här är ett exempel:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Sätt en brytpunkt genom att klicka till vänster om radnumret i Xcode och kör programmet. När det når brytpunkten pausar Xcode exekveringen. Nu kan du:

1. Kontrollera variabelvärden.
2. Stega över (kör nästa rad) eller stega in (gå in i en funktion) med hjälp av felsökningskontrollerna.
3. Lägga till uttryck till "bevakningslistan" för att övervaka ändringar i specifika variabler eller konstanter.

Här är vad du kanske ser i felsökningsområdet:

```
(lldb) po number
5
(lldb) po result
120
```

## Djupdykning:
Felsökare har varit en del av programmeringslandskapet sedan 1940-talet, och har utvecklats från enkla brytpunkssystem till komplexa, UI-drivna upplevelser. Andra alternativ förutom Xcodes inbyggda felsökare inkluderar tredjepartsverktyg som LLDB (Low Level Debugger) som Xcode använder under huven. Vissa personer felsöker även med `print()`-utskrifter (kärt kallade "grottmannafelsökning"), men detta är mindre effektivt för stora projekt eller komplexa buggar. När du använder en felsökare hanterar du exekveringskontroll, körningstidsintrospektion och datamanipulation. En djup förståelse av dessa principer bidrar mycket till effektiv felsökning.

## Se även:
- [Apples Xcode-felsökningsguide](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDB:s snabbstartguide](https://lldb.llvm.org/use/tutorial.html)
- [Ray Wenderlichs Swift-felsökningstutorial](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
