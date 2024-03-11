---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:39.984767-07:00
description: "I Visual Basic for Applications (VBA) er det \xE5 hente den gjeldende\
  \ datoen en vanlig oppgave som gj\xF8r at programmerere kan jobbe dynamisk med datoer\
  \ i sine\u2026"
lastmod: '2024-03-11T00:14:14.171331-06:00'
model: gpt-4-0125-preview
summary: "I Visual Basic for Applications (VBA) er det \xE5 hente den gjeldende datoen\
  \ en vanlig oppgave som gj\xF8r at programmerere kan jobbe dynamisk med datoer i\
  \ sine\u2026"
title: "F\xE5 den gjeldende datoen"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

I Visual Basic for Applications (VBA) er det å hente den gjeldende datoen en vanlig oppgave som gjør at programmerere kan jobbe dynamisk med datoer i sine makroer eller applikasjoner. Denne funksjonaliteten er avgjørende for operasjoner som logging, tidsstempeltransaksjoner eller å foreta beregninger basert på datoer.

## Hvordan:

Å hente den gjeldende datoen i VBA er greit, ved å bruke `Date`-funksjonen, mens `Now`-funksjonen gir både den gjeldende datoen og tiden. Slik kan du jobbe med begge:

```vb
Sub GetCurrentDate()
    ' Bruker Date-funksjonen for å få den gjeldende datoen
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Gjeldende Dato: "; currentDate
    
    ' Bruker Now-funksjonen for å få den gjeldende datoen og tiden
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Gjeldende Dato og Tid: "; currentDateTime
End Sub
```

Når du kjører denne makroen, skriver `Debug.Print`-metoden ut den gjeldende datoen og den gjeldende datoen og tiden til Immediate-vinduet i VBA-editoren. For eksempel:

```
Gjeldende Dato: 12.4.2023
Gjeldende Dato og Tid: 12.4.2023 3:45:22 PM
```

Husk at datoformatet kan variere basert på systeminnstillingene til brukerens datamaskin.

## Dypdykk

`Date`- og `Now`-funksjonene innkapsler kompleksiteten av å håndtere dato og tid i Visual Basic for Applications, og gir en applikasjonsnivå-abstraksjon som gjør arbeidet med datoer enkelt og intuitivt. Historisk sett har det å håndtere dato og tid i programmering vært fullt av utfordringer, inkludert håndtering av forskjellige tidssoner, endringer i sommertid og ulike datoformater.

I VBA er disse funksjonene avhengige av det underliggende systemets dato og tid, noe som betyr at de påvirkes av brukerens lokalitet og systeminnstillinger. Det er et tveegget sverd som sikrer konsistens med brukerens miljø, men som også krever nøye håndtering av lokalisering og justering av tidssone i globale applikasjoner.

Selv om VBAs dato- og tidsfunksjoner er helt passende for mange applikasjoner, spesielt innenfor omfanget av Office-automatisering, kan de mangle presisjonen eller granulariteten som kreves for mer komplekse applikasjoner som høyfrekvente handelssystemer eller vitenskapelige simuleringer. I slike tilfeller kan andre programmeringsmiljøer eller språk som Python eller C# tilby mer sofistikerte biblioteker for manipulering av dato og tid.

Likevel, for det store flertallet av oppgaver som involverer datoer og tider i konteksten av Excel, Word eller andre Office-applikasjoner, tilbyr VBAs `Date`- og `Now`-funksjoner en balanse mellom enkelhet, ytelse og brukervennlighet som er vanskelig å slå.
