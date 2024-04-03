---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:28.117074-07:00
description: "Hvordan: I VBA bruker du prim\xE6rt funksjonene `Mid`, `Left`, og `Right`\
  \ for \xE5 trekke ut delstrenger. Nedenfor utforsker vi disse funksjonene med eksempler:\u2026"
lastmod: '2024-03-13T22:44:40.606176-06:00'
model: gpt-4-0125-preview
summary: "I VBA bruker du prim\xE6rt funksjonene `Mid`, `Left`, og `Right` for \xE5\
  \ trekke ut delstrenger."
title: Uttrekking av delstrenger
weight: 6
---

## Hvordan:
I VBA bruker du primært funksjonene `Mid`, `Left`, og `Right` for å trekke ut delstrenger. Nedenfor utforsker vi disse funksjonene med eksempler:

1. **Mid**: Trekker ut en delstreng fra en streng som starter på en spesifisert posisjon.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Mid(exampleString, 7, 5)
   Debug.Print result  ' Utdata: World
   ```

2. **Left**: Trekker ut en delstreng fra venstre side av strengen, opp til et spesifisert antall tegn.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Left(exampleString, 5)
   Debug.Print result  ' Utdata: Hello
   ```

3. **Right**: Trekker ut en delstreng fra høyre side av strengen, opp til et spesifisert antall tegn.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Right(exampleString, 5)
   Debug.Print result  ' Utdata: World
   ```

Disse grunnleggende funksjonene utgjør grunnlaget for ekstraksjon av delstrenger i VBA, og tilbyr robuste og greie tilnærminger til strengmanipulasjon.

## Dypdykk:
Historisk sett har evnen til å manipulere strenger i programmering vært essensiell, med BASIC (forgjengeren til VBA) blant de første til å demokratisere denne funksjonaliteten i de tidlige dagene av personlig databehandling. Funksjonene `Mid`, `Left`, og `Right` i VBA arver denne arven, og tilbyr et forenklet grensesnitt for moderne programmerere.

Selv om disse funksjonene er ganske effektive for mange oppgaver, har fremveksten av Regulære Uttrykk i nyere språk gitt en mer kraftfull og fleksibel måte å arbeide med tekst på. Til tross for dette, gjør den øyeblikkelige enkelheten og tilgjengeligheten til de tradisjonelle VBA delstrengfunksjonene dem perfekt egnet for raske oppgaver og de som er nye i programmering.

For mer komplekse analyse- og søkeoperasjoner innenfor strenger, støtter VBA også mønstersøking gjennom `Like`-operatøren og Regulære Uttrykk via `VBScript.RegExp`-objektet, selv om disse krever litt mer oppsett og forståelse for å bruke effektivt. Mens disse verktøyene tilbyr større kraft, sikrer den greie naturen til `Mid`, `Left`, og `Right` deres fortsatte relevans og nytte i mange VBA-programmer.
