---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:17.848297-07:00
description: "\xC5 konvertere en streng til sm\xE5 bokstaver inneb\xE6rer \xE5 transformere\
  \ alle store bokstaver i en streng til deres sm\xE5 bokstav-ekvivalenter. Denne\
  \ prosessen er\u2026"
lastmod: '2024-03-11T00:14:14.143427-06:00'
model: gpt-4-0125-preview
summary: "\xC5 konvertere en streng til sm\xE5 bokstaver inneb\xE6rer \xE5 transformere\
  \ alle store bokstaver i en streng til deres sm\xE5 bokstav-ekvivalenter. Denne\
  \ prosessen er\u2026"
title: "Konvertere en streng til sm\xE5 bokstaver"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver innebærer å transformere alle store bokstaver i en streng til deres små bokstav-ekvivalenter. Denne prosessen er essensiell for ulike programmeringsoppgaver, inkludert datanormalisering, casesensitive sammenligninger og forbedring av konsistens i brukerinndata.

## Hvordan:

I Visual Basic for Applications (VBA) er konvertering av en streng til små bokstaver enkel ved bruk av `LCase`-funksjonen. Denne funksjonen tar en streng som inndata og returnerer en ny streng der alle store bokstaver er konvertert til små bokstaver. Her er et enkelt eksempel for å illustrere dette:

```basic
Dim originalString As String
Dim lowerCaseString As String

originalString = "Hello, World!"
lowerCaseString = LCase(originalString)

Debug.Print lowerCaseString ' Resultat: hello, world!
```

Du kan også bruke `LCase` direkte i sammenligninger eller tildelinger for mer strømlinjeformet kode:

```basic
If LCase(userInput) = "yes" Then
    Debug.Print "Brukeren sa ja"
End If
```

Dette andre eksempelet viser hvordan håndtere brukerinndata på en casesensitiv måte ved å konvertere inndataen til små bokstaver før sammenligning.

## Dypdykk

`LCase`-funksjonen understøtter strengmanipulasjon i VBA og har vært en nøkkelfunksjon siden språkets begynnelse. Den forenkler oppgaver relatert til bokstavkonvertering, som er vanlige i datasortering og prosessering av brukerinndata. Selv om `LCase` effektivt møter behovet for å konvertere tegn til små bokstaver i ulike applikasjoner, er det også viktig å anerkjenne begrensningene og alternativene.

For eksempel, mens `LCase` fungerer problemfritt for det engelske alfabetet, kan håndtering av språk med mer komplekse regler for bokstavstørrelse kreve ytterligere hensyn eller bruk av `StrConv`-funksjonen med passende lokaliseringer for bokstavkonvertering.

Videre, når man går over fra språk som Python, hvor `str.lower()` brukes, eller JavaScript, med sin `string.toLowerCase()`, kan programmerere finne `LCase` enkelt, men bør holde i mente VBA’s særegenheter, slik som mangel på metodekjeding.

Oppsummert, selv om det er nyere og potensielt kraftigere alternativer i andre språk, forblir `LCase` en pålitelig og enkel-å-bruke funksjon for å konvertere strenger til små bokstaver i VBA, og passer godt inn i språkets overordnede syntaks og funksjonalitetsskjema.
