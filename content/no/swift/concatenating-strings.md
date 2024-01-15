---
title:                "Sammenkobling av strenger"
html_title:           "Swift: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er en erfaren programmerer eller en nybegynner, er sannsynligheten stor for at du vil støte på behovet for å slå sammen flere tekststrenger på et eller annet tidspunkt. Dette kan være for å lage en kompleks beskjed, en URL eller bare for å lage en generell tekstutdata. Uansett hva grunnen måtte være, er det viktig å vite hvordan man kan utføre dette effektivt og enkelt i Swift.

## Hvordan gjøre det

Det er flere måter å slå sammen strings (tekststrenger) i Swift, men den enkleste og mest effektive er ved hjelp av operatøren "+".
```Swift
let navn = "Mari"
let alder = 25
let beskjed = navn + " er " + "\(alder)" + " år gammel."
print(beskjed)
```
Output: Mari er 25 år gammel.

Her kan du se at vi enkelt kombinerer tre forskjellige typer data (tekst, tall og en uttrykksstreng) til en eneste beskjed ved hjelp av "+" operatøren. Dette er en rask og enkel måte å slå sammen strings på i Swift. Du kan også bruke operatøren til å legge til flere strings på en gang:
```Swift
let beskjed = "Hei" + " verden" + "!"
print(beskjed)
```
Output: Hei verden!

## Deep Dive

I tillegg til å bruke "+" operatøren, kan du også bruke funksjonen "String(format:)" for å slå sammen strings. Denne funksjonen lar deg bruke formateringsmønstre, som gjør det enkelt å sette sammen tekststrenger på en spesifikk måte. For eksempel:

```Swift
let navn = "Anna"
let alder = 32
let beskjed = String(format: "%@ er %d år gammel.", navn, alder)
print(beskjed)
```
Output: Anna er 32 år gammel.

I dette tilfellet bruker vi formateringsmønstrene "%@" for tekst og "%d" for tall, og erstatter de med variablene våre. Dette kan være nyttig hvis du trenger å slå sammen en større mengde tekststrenger og ønsker mer kontroll over hvordan de skal presenteres.

## Se også

- [Offisiell Swift Dokumentasjon - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorialspoint - Swift string concatenation](https://www.tutorialspoint.com/swift-programming/swift_string_concatenation.htm)
- [Hacking with Swift - How to concatenate strings to make one joined string](https://www.hackingwithswift.com/example-code/strings/how-to-concatenate-strings-to-make-one-joined-string)