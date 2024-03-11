---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:35.075274-07:00
description: "Het lezen van commandoregelargumenten stelt je Swift-programma in staat\
  \ om extra details te grijpen wanneer gebruikers het uitvoeren. Dit is belangrijk\u2026"
lastmod: '2024-03-11T00:14:25.011415-06:00'
model: gpt-4-0125-preview
summary: "Het lezen van commandoregelargumenten stelt je Swift-programma in staat\
  \ om extra details te grijpen wanneer gebruikers het uitvoeren. Dit is belangrijk\u2026"
title: Commandoregelargumenten lezen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het lezen van commandoregelargumenten stelt je Swift-programma in staat om extra details te grijpen wanneer gebruikers het uitvoeren. Dit is belangrijk omdat het maatwerk en controle toevoegt zonder de noodzaak van gebruikersinteractie terwijl het programma draait.

## Hoe te:

Swift maakt het lezen van commandoregelargumenten super eenvoudig. Ze zijn toegankelijk via de `CommandLine` structuur. Hier is de essentie:

```swift
for argument in CommandLine.arguments {
    print(argument)
}
```

Als je dit in een `main.swift` bestand gooit en je programma draait met wat extra tekst, zoals `swift run YourProgram foo bar`, zal je output er zo uitzien:

```
/path/to/YourProgram
foo
bar
```

Dat is elk argument uitgeprint, inclusief het pad naar je programma als het eerste element - onthoud dat altijd!

## Diepe Duik

Historisch gezien zijn commandoregelargumenten een basisgeweest in programmering, waardoor mensen het gedrag van een programma kunnen aanpassen zonder code te veranderen. Het is de erfenis van Unix, en vrijwel alle talen ondersteunen deze functie.

In Swift is `CommandLine.arguments` een array van strings, met elk element een stukje van je invoer, gesplitst door witruimte. Deze array wordt door het besturingssysteem overhandigd wanneer je programma start; Swift maakt het gewoon makkelijk toegankelijk.

Naast `CommandLine.arguments`, kun je dieper duiken in meer complexe parsing met bibliotheken zoals `Swift Argument Parser` voor zwaarder werk. Dit is handig wanneer je meer nodig hebt dan alleen eenvoudige invoer - denk aan vlaggen, opties en subcommando's.

Wat betreft implementatie, die commandoregelargumenten komen bij je via een C-array achter de schermen - goede oude `argc` en `argv`. Swift houdt het verborgen, maar behoudt nog steeds hetzelfde basisgedrag dat je zou vinden in C of C++.

## Zie Ook

- Voor een brede kijk op commandoregelprogramma's in Swift, bekijk de [Swift.org Documentatie](https://swift.org/getting-started/#using-the-package-manager).
- Om je argument parsing spel te verbeteren, ga naar de [Swift Argument Parser GitHub repo](https://github.com/apple/swift-argument-parser) voor meer geavanceerde opstellingen.
- Als je nieuwsgierig bent hoe andere talen dit aanpakken, probeer dit dan te vergelijken met Python's `sys.argv` of Node's `process.argv`.
