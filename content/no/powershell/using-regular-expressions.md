---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulære uttrykk er søkemønstre som brukes for å matche tekststrenger. Programmerere bruker det for å finne, erstatte, og validere tekst raskt og konsist.

## How to:

### Finne en match
```PowerShell
$text = 'Finn kode: PS-1234'
$pattern = 'PS-\d+'
if ($text -match $pattern) {
    "Match funnet: $($matches[0])"
}
```
#### Output
```
Match funnet: PS-1234
```

### Erstatte tekst
```PowerShell
$nyTekst = $text -replace $pattern, 'NO-4321'
"Erstattet tekst: $nyTekst"
```
#### Output
```
Erstattet tekst: Finn kode: NO-4321
```

### Validere e-post
```PowerShell
$epost = 'bruker@example.com'
$epostMønster = '^\S+@\S+\.\S+$'
if ($epost -match $epostMønster) {
    "E-posten er valid."
} else {
    "E-posten er ikke valid."
}
```
#### Output
```
E-posten er valid.
```

## Deep Dive

Regular expressions, eller "regex", har sin historie helt tilbake til 1950-tallet. Alternativene til regex inkluderer tekstbehandling ved hjelp av vanlige strengefunksjoner eller parsere, som kan være tregere og mindre fleksible. Regex i PowerShell bruker .NET sitt regex-bibliotek, noe som gjør det kraftig og i stand til komplekse søkeoperasjoner.

## See Also

- RegExr, et online verktøy for å lære, bygge, og teste regulære uttrykk: [RegExr](https://regexr.com/)
- PowerShell sin offisielle dokumentasjonsside: [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
