---
title:                "Å bruke regulære uttrykk"
html_title:           "Go: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å bruke regulære uttrykk er en måte for programmerere å søke etter og manipulere tekst på en effektiv måte. Det er spesielt nyttig når man ønsker å finne mønstre i store mengder tekst eller å gjøre omformatering av data. Regulære uttrykk finnes i de fleste programmeringsspråk, inkludert Go, og er et viktig verktøy for å strukturere og prosessere tekst.

## Slik gjør du det:

For å bruke regulære uttrykk i Go, kan du bruke "regexp" pakken. Først må du lage et regulært uttrykk ved å bruke funksjonen "Compile" og angir mønsteret du ønsker å finne. Deretter kan du bruke funksjoner som "Match" eller "FindString" for å finne matchende tekst eller "ReplaceAllString" for å endre teksten.

```Go
re := regexp.Compile("a+[b-z]+c+")
matches := re.FindString("abcdefg") // matches = "abc"
newString := re.ReplaceAllString("abcdefg", "123") // newString = "123defg"
```

## Dykk dypere:

Regulære uttrykk ble først introdusert i programmeringsspråket SNOBOL i 1960-årene. De har siden blitt en viktig del av programmering og finnes i dag i de fleste programmeringsspråk. Noen alternativer til regulære uttrykk er string-manipuleringsfunksjoner som "Split" og "Replace" i Go eller "grep" kommandoen i terminalen. Det er også verdt å merke seg at noen mønstre kan være vanskelige å uttrykke med regulære uttrykk og kan kreve mer komplekse algoritmer.

## Se også:

- https://golang.org/pkg/regexp/
- https://regexr.com/
- https://www.regular-expressions.info/