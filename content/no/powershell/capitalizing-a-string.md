---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kapitalisering av en streng betyr å gjøre første bokstav stor i ordet eller hele strengen. Programmerere bruker det for å standardisere tekst, for eksempel for å sikre at navn eller titler vises konsistent.

## Hvordan gjøre det:
Du kan kapitalisere strenger i PowerShell med `.ToUpper()`, `.ToLower()` og `[CultureInfo]::CurrentCulture.TextInfo.ToTitleCase()`. Her er noen raske eksempler:

```PowerShell
# Gjøre hele strengen stor:
$streng = "heisann, dette er en test."
$storStreng = $streng.ToUpper()
$storStreng # output: HEISANN, DETTE ER EN TEST.

# Gjøre hele strengen liten:
$litenStreng = $streng.ToLower()
$litenStreng # output: heisann, dette er en test.

# Kapitaliser hvert ord:
$tekstInfo = [CultureInfo]::CurrentCulture.TextInfo
$titulertStreng = $tekstInfo.ToTitleCase($streng)
$titulertStreng # output: Heisann, Dette Er En Test.
```

## Dypdykk:
Historisk sett var strengmanipulering essensielt i tidlige datasystemer for å behandle tekstdata. I PowerShell gir `.ToUpper()` og `.ToLower()` enkel tilgang til å endre store og små bokstaver. `[CultureInfo]::CurrentCulture.TextInfo.ToTitleCase()` tar hensyn til kulturelle konvensjoner og er bedre for å gjøre bokstavene i starten av hvert ord store, selv om den ikke fungerer på allerede kapitaliserte strenger perfekt.

Alternativer for kapitalisering inkluderer manuell iterasjon over tegnene i strengen, men dette er ikke praktisk i PowerShell der innebygde metoder er tilgjengelige.

Når det gjelder implementasjon, fungerer `.ToUpper()` og `.ToLower()` på tegnnivå og endrer ikke strengens lengde, mens `ToTitleCase()` bruker ordmønstregjenkjenning og kan bli mer komplisert, avhengig av språkets regler.

## Se Også:
- [.NET Globalization and Localization](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)
