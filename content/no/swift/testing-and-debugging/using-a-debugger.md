---
date: 2024-01-26 04:10:55.172891-07:00
description: "For \xE5 bruke feils\xF8keren i Xcode (IDE for Swift), kan du sette\
  \ brytepunkter, inspisere variabler og se p\xE5 uttrykk. Her er et eksempel: ```Swift\
  \ func\u2026"
lastmod: '2024-03-13T22:44:41.146440-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 bruke feils\xF8keren i Xcode (IDE for Swift), kan du sette brytepunkter,\
  \ inspisere variabler og se p\xE5 uttrykk. Her er et eksempel: ```Swift func\u2026"
title: "\xC5 bruke en debugger"
weight: 35
---

## Hvordan:
For å bruke feilsøkeren i Xcode (IDE for Swift), kan du sette brytepunkter, inspisere variabler og se på uttrykk. Her er et eksempel:

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

Sett et brytepunkt ved å klikke til venstre for et linjenummer i Xcode, og kjør programmet. Når det treffer brytepunktet, pauser Xcode utførelsen. Nå kan du:

1. Sjekke variabelverdier.
2. Steg over (kjør neste linje) eller steg inn (gå inn i en funksjon) ved hjelp av feilsøkerkontrollene.
3. Legge til uttrykk i 'overvåkningslisten' for å overvåke endringer på bestemte variabler eller konstanter.

Dette er hva du kan se i feilsøkerområdet:

```
(lldb) po number
5
(lldb) po result
120
```

## Dypdykk:
Feilsøkere har vært en del av programmeringslandskapet siden 1940-tallet, og har utviklet seg fra enkle brytepunktsystemer til komplekse, UI-drevne opplevelser. Andre alternativer enn Xcodes innebygde feilsøker inkluderer tredjepartsverktøy som LLDB (Low Level Debugger), som Xcode bruker under panseret. Noen feilsøker til og med med `print()`-påstander (kjærlig kjent som "hulemannsdebugging"), men dette er mindre effektivt for store prosjekter eller komplekse feil. Når du bruker en feilsøker, balanserer du utførelseskontroll, kjøretidsintrospeksjon og datamanipulasjon. En dyp forståelse av disse prinsippene kommer langt i effektiv feilsøking.

## Se også:
- [Apples Xcode-feilsøkingsguide](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDB Rask startguide](https://lldb.llvm.org/use/tutorial.html)
- [Ray Wenderlichs Swift-feilsøkingstutorial](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
