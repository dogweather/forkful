---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:09.398041-07:00
description: "Hvordan: I VBA blir feilh\xE5ndtering typisk implementert ved bruk av\
  \ `On Error`-setningen som instruerer VBA i hvordan man skal fortsette n\xE5r en\
  \ feil\u2026"
lastmod: '2024-03-13T22:44:40.628706-06:00'
model: gpt-4-0125-preview
summary: "I VBA blir feilh\xE5ndtering typisk implementert ved bruk av `On Error`-setningen\
  \ som instruerer VBA i hvordan man skal fortsette n\xE5r en feil oppst\xE5r."
title: "H\xE5ndtering av feil"
weight: 16
---

## Hvordan:
I VBA blir feilhåndtering typisk implementert ved bruk av `On Error`-setningen som instruerer VBA i hvordan man skal fortsette når en feil oppstår. De mest vanlige feilhåndteringsstrategiene involverer `On Error GoTo`-merket, `On Error Resume Next`, og `On Error GoTo 0`.

**Eksempel 1: Bruk av `On Error GoTo`**

Denne tilnærmingen lar deg dirigere programmet til en spesifikk seksjon av koden, merket umiddelbart etter at en feil er oppstått.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' Dette vil forårsake en dele med null-feil

    Exit Sub
ErrHandler:
    MsgBox "En feil oppstod: " & Err.Description, vbCritical, "Feil!"
    Resume Next
End Sub
```

I dette eksemplet vil enhver kjøretidsfeil utløse et hopp til `ErrHandler`, som viser en feilmelding og deretter fortsetter med neste linje etter feilen.

**Eksempel 2: Bruk av `On Error Resume Next`**

Denne strategien instruerer VBA til å fortsette å utføre neste linje med kode selv om en feil oppstår, hvilket kan være nyttig for feil som forventes å være harmløse eller når du planlegger å håndtere feilen senere i utførelsen.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' Dette vil ikke forårsake at programmet stopper; feilen ignoreres
    
    ' Sjekk om en feil har oppstått
    If Err.Number <> 0 Then
        MsgBox "En feil oppstod: " & Err.Description, vbExclamation, "Håndtert Feil"
        ' Tilbakestill feil
        Err.Clear
    End If
End Sub
```

I dette tilfellet brytes ikke programmet på grunn av feilen; det sjekkes om en feil har oppstått, håndterer den hvis det har skjedd, og deretter tilbakestilles feilen.

## Dypdykk
Historisk sett har feilhåndtering i programmeringsspråk utviklet seg fra enkle goto-setninger til mer sofistikerte mekanismer som unntak i språk som Java og C#. VBAs feilhåndtering, selv om den ikke er like kraftig eller fleksibel som moderne unntakshåndtering, tjener sitt formål innenfor konteksten av språkets anvendelse i automatisering av oppgaver i Microsoft Office-miljøer.

Den primære begrensningen til VBAs feilhåndtering ligger i dens noe tungvinte og manuelle tilnærming, som krever nøye plassering av feilhåndteringskode og klar forståelse av utførelsesflyten. Moderne programmeringsspråk tilbyr typisk mer elegante løsninger, som try-catch-blokker, som automatisk håndterer flyten til feilhåndteringskoden uten behov for manuelle sjekker eller hopp i kodeutførelsen.

Til tross for disse begrensningene, er VBAs feilhåndteringsmekanismer passende for de fleste automatiseringsoppgaver og når de brukes riktig, kan de betydelig redusere sannsynligheten for at uhåndterte feil forårsaker problemer for brukerne. I tillegg kan forståelsen av VBAs feilhåndtering gi innsikt i eldre programmeringsparadigmer og utviklingen av feilhåndteringsstrategier i programvareutvikling.
