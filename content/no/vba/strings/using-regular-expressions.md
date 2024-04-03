---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:56.520377-07:00
description: "Regul\xE6re uttrykk (regex) i Visual Basic for Applications (VBA) tilbyr\
  \ en kraftfull m\xE5te \xE5 s\xF8ke, matche og manipulere strenger p\xE5. Programmerere\
  \ bruker dem\u2026"
lastmod: '2024-03-13T22:44:40.607408-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk (regex) i Visual Basic for Applications (VBA) tilbyr\
  \ en kraftfull m\xE5te \xE5 s\xF8ke, matche og manipulere strenger p\xE5."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:
For å bruke regulære uttrykk i VBA, må du først aktivere Microsoft VBScript Regular Expressions-biblioteket. I VBA-editoren, gå til `Verktøy` -> `Referanser`, og deretter kryss av for `Microsoft VBScript Regular Expressions 5.5`.

Her er et grunnleggende eksempel for å finne ut om et mønster finnes i en streng:

```vb
Sub FinnMønster()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' Ser etter ordet "is"
    End With
    
    Dim testStreng As String
    testStreng = "Dette er en teststreng."
    
    If regex.Test(testStreng) Then
        MsgBox "Mønster funnet."
    Else
        MsgBox "Mønster ikke funnet."
    End If
End Sub
```

For å erstatte et mønster i en streng:

```vb
Sub ErstattMønster()
    Dim regex As Object, erstattetStreng As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' Matcher ethvert mellomromstegn
    End With
    
    erstattetStreng = regex.Replace("Dette er en teststreng.", "_")
    MsgBox erstattetStreng  ' Resultat: "Dette_er_en_teststreng."
End Sub
```

## Dypdykk
Inkluderingen av regulære uttrykk i programmeringsspråk kan ofte spores tilbake til Unix-verktøy fra 1970-tallet. VBA integrerte regex gjennom VBScript Regular Expressions-biblioteket, noe som understreker dets betydning i teksthåndteringsoppgaver selv i applikasjoner ikke typisk assosiert med tung teksthåndtering som Excel eller Access.

Til tross for deres styrke, kan regex i VBA noen ganger være mindre intuitivt eller ytelsessterkt sammenlignet med mer moderne implementasjoner i språk som Python eller JavaScript. For eksempel, Pythons `re`-modul tilbyr omfattende støtte for navngitte grupper og mer sofistikerte mønstersøkefunksjoner, som gir en renere og potensielt mer lesbar tilnærming. Imidlertid, når man arbeider innenfor VBA-økosystemet, forblir regulære uttrykk et uvurderlig verktøy for oppgaver som krever mønstersøk eller tekstmanipulasjon. Effektivitetsofferet er ofte ubetydelig i lys av bekvemmeligheten og mulighetene regex bringer til bordet når man håndterer strenger i Office-applikasjoner.
