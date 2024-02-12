---
title:                "Het gebruik van een interactieve shell (REPL)"
aliases:
- nl/swift/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:07.465236-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een interactieve shell gebruiken, of een Read-Eval-Print Loop (REPL), stelt je in staat om interactief te coderen. Programmeurs gebruiken het om snel Swift-snippets te testen, te debuggen of de taal te leren.

## Hoe te:
Roep REPL op door een terminal te openen en `swift` te draaien. Typ direct code in en druk op Enter om het uit te voeren. Hier is een voorproefje:

```Swift
1> let greeting = "Hallo, REPL!"
greeting: String = "Hallo, REPL!"
2> print(greeting)
Hallo, REPL!
```

Afsluiten met `:quit` of `Control-D`.

## Diepgaande duik
De wortels van REPL gaan terug tot de Lisp-interpreters in de jaren '60. Swift's REPL bevindt zich bovenop LLVM, een krachtig compilerframework, en biedt meer dan alleen basisinterpretatie - het is een volwaardige tool met autocomplete, debugging en meer. REPL is geweldig voor leren of prototyping, maar het is geen op zichzelf staande ontwikkelomgeving. Sommige mensen geven de voorkeur aan het gebruik van Playgrounds in Xcode voor een meer grafische, op bestanden gebaseerde aanpak, terwijl anderen zich houden aan traditionele scriptbewerking en -uitvoering.

Onder de motorkap compileert Swift's REPL dynamisch code naar machinetaal en voert deze uit, waardoor het relatief snel is. Het kan ook toegang krijgen tot gecompileerde Swift-modules, of zelfs C-bibliotheken, wat het redelijk krachtig maakt. Let wel, niet alles werkt perfect in REPL; sommige Swift-functies, vooral die welke complexe projectinstellingen of storyboard-bestanden vereisen, zullen hier niet werken.

## Zie ook
- [Swift.org - Aan de slag](https://www.swift.org/getting-started/#using-the-repl)
- Apple’s [Introductie tot Xcode Playgrounds](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM Project](https://llvm.org/)
