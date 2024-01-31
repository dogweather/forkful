---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:09:54.747361-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zijn patronen die gebruikt worden om combinaties van karakters in strings te vinden. Programmeurs gebruiken ze voor het zoeken, valideren en manipuleren van tekst, waardoor ze een Zwitsers zakmes voor stringbewerkingen zijn.

## Hoe te:
```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Voorbeeld: E-mails vinden in een string
    text := "Neem contact op via contact@example.com of support@random.org"
    emailRegex := regexp.MustCompile(`[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}`)

    // FindString geeft de eerste match terug
    fmt.Printf("Eerste e-mail: %s\n", emailRegex.FindString(text)) 
    // Uitvoer: Eerste e-mail: contact@example.com

    // FindAllString geeft alle matches terug
    emails := emailRegex.FindAllString(text, -1)
    fmt.Printf("Alle e-mails: %v\n", emails) 
    // Uitvoer: Alle e-mails: [contact@example.com support@random.org]

    // Tekst vervangen
    sanitizedText := emailRegex.ReplaceAllString(text, "[geredigeerd]")
    fmt.Println(sanitizedText) 
    // Uitvoer: Neem contact op via [geredigeerd] of [geredigeerd]
}
```

## Diepgaande duik
Regex heeft zijn oorsprong in Unix in de jaren '50 en kreeg tractie door tools zoals `grep`. Perl maakte ze later populair. Alternatieven omvatten het gebruik van stringfuncties of parsers voor eenvoudige en gestructureerde gegevens, respectievelijk. Wat de implementatie betreft, is het `regexp` pakket van Go NFA-gebaseerd (niet-deterministische eindige automaat), wat regex efficiënt afhandelt zonder de terugloopvalkuilen die in sommige andere motoren worden gevonden.

## Zie ook
- Documentatie van het Go `regexp` pakket: [pkg.go.dev/regexp](https://pkg.go.dev/regexp)
- Online regex tester en debugger: [regex101.com](https://regex101.com/)
- Mozilla Developer Network regex handleiding: [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
