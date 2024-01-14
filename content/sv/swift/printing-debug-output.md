---
title:                "Swift: Utskrift av felsökningsutdata"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför debugga utdata är viktigt i Swift

När man utvecklar en app i Swift kan det vara lätt att fastna i problem och fel som uppstår. Att lägga till print-satser för att skriva ut information vid olika steg i koden kan vara en användbar metod för att förstå vad som händer och varför saker inte fungerar som de ska. Detta kan spara tid och frustration under utvecklingsprocessen.

## Så här använder du dig av debug utdata i Swift

För att skriva ut debug utdata i Swift, används funktionen `print()` med den information som önskas i parenteser. Här är ett exempel på hur du kan skriva ut en variabel `name` och dess värde:

```Swift
let name = "Johan"
print("Namnet är: \(name)")
```

Detta kommer att skriva ut följande i konsolfönstret:

```
Namnet är: Johan
```

För att skriva ut flera variabler vid ett tillfälle, kan du använda den speciella funktionen `debugPrint()` och skriva alla variabler i parenteser separerade med kommatecken.

```Swift
let age = 28
let occupation = "Programmerare"
debugPrint(age, occupation)
```

Detta kommer att skriva ut:

```
28, "Programmerare"
```

## Djupare utforskning av debug utdata i Swift

Utöver att skriva ut värden av variabler, kan man även använda `print()` för att skriva ut textsträngar eller meddelanden som hjälper till att förklara vad som händer i koden. Du kan också använda `print()` tillsammans med villkorsuttryck för att se vilka vägar i koden som faktiskt utförs.

För mer avancerad debuggning, kan du använda funktionen `assert()`. Denna funktion tar ett villkorsuttryck som måste vara sant, annars kommer den att skriva ut ett felmeddelande i konsolen och stoppa execution av koden.

För att lättare läsa och hålla reda på utskrifterna, kan du även använda dig av breakpoints och Xcode's Debug Navigator.

## Se även

* [Apple's dokumentation om debug utdata i Swift](https://developer.apple.com/library/content/documentation/ToolsLanguages/Conceptual/Xcode_Overview/DebugYourApp/DebugYourApp.html)
* [En guide till debug utdata i Swift](https://www.hackingwithswift.com/read/16/4/adding-debugging-println-statements)
* [Video tutorial om debug utdata i Swift](https://www.youtube.com/watch?v=qYx_VIHS_xI)