---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:49.695283-07:00
description: "De lengte van een string bepalen in Go gaat over het vaststellen van\
  \ het aantal karakters dat het bevat. Programmeurs voeren deze bewerking routinematig\u2026"
lastmod: '2024-03-13T22:44:50.278003-06:00'
model: gpt-4-0125-preview
summary: "De lengte van een string bepalen in Go gaat over het vaststellen van het\
  \ aantal karakters dat het bevat. Programmeurs voeren deze bewerking routinematig\u2026"
title: De lengte van een string vinden
---

{{< edit_this_page >}}

## Wat & Waarom?
De lengte van een string bepalen in Go gaat over het vaststellen van het aantal karakters dat het bevat. Programmeurs voeren deze bewerking routinematig uit om strings effectief te manipuleren, of het nu gaat om validatie, het extraheren van subteksten, of simpelweg om beperkingen op te leggen aan gebruikersinvoer.

## Hoe te:
In Go worden strings behandeld als onveranderlijke bytesequenties. Je kunt de lengte van een string vinden met behulp van de ingebouwde `len()` functie die het aantal bytes teruggeeft, niet noodzakelijkerwijs het aantal karakters. Hier is hoe je het gebruikt:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Gebruikmaken van len() om de bytelengte te vinden
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Byte Lengte:", byteLength) // Uitvoer: Byte Lengte: 13

	// Om nauwkeurig het aantal karakters of runes in een string te krijgen
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Rune Lengte:", runeLength) // Uitvoer: Rune Lengte: 9
}
```
De eerste methode die `len()` gebruikt, geeft niet altijd het verwachte resultaat, aangezien het bytes telt. Voor strings die niet-ASCII karakters bevatten (zoals "世界"), moet `RuneCountInString` uit het `unicode/utf8` pakket in plaats daarvan gebruikt worden om Unicode codepunten nauwkeurig te tellen.

## Diepduiken
Voor Go 1 was er geen strikte scheiding voor het behandelen van strings als sequenties van bytes versus sequenties van karakters. Na Go 1 noodzaakte de adoptie van UTF-8 als de standaardcoderingsschema voor strings duidelijkere benaderingen. De `len()` functie werkt perfect voor ASCII-strings, waar karakters in een enkele byte worden gerepresenteerd. Echter, naarmate Go-applicaties globaler werden en de behoefte om een veelvoud aan talen en karaktersets te ondersteunen groeide, toonde de simplistische aanpak van `len()` beperkingen.

De introductie en het gebruik van `utf8.RuneCountInString()` beantwoorden deze beperkingen door een manier te bieden om daadwerkelijke Unicode-karakters (runes in Go-terminologie) te tellen. Deze methode zorgt ervoor dat de lengteberekening onafhankelijk is van de coderingsspecifieke eigenschappen van UTF-8, waar karakters meerdere bytes kunnen beslaan.

Een alternatieve benadering voor het traverseren en manipuleren van strings, meer in lijn met Go’s ethos van gelijktijdigheid en efficiëntie, zou kunnen inhouden dat strings worden behandeld als slices van runes. Deze methode vereist echter een conversiestap en lost niet direct alle complexiteiten van Unicode op (bijv. combinerende karakters).

Samenvattend, terwijl `len()` geschikt is voor bytelengte en efficiënt voor ASCII-tekst, is `utf8.RuneCountInString()` een betrouwbaardere keuze voor een wereldwijd compatibele applicatie. Toch worden ontwikkelaars aangemoedigd om de afwegingen in prestaties en geheugengebruik die deze keuzes met zich meebrengen te begrijpen.
