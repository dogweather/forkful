---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:06.340247-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel i Visual Basic for Applications (VBA)\
  \ inneb\xE6rer programmatisk tilgang til webressurser eller webtjenester ved \xE5\
  \ gj\xF8re\u2026"
lastmod: '2024-03-13T22:44:40.615740-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel i Visual Basic for Applications (VBA)\
  \ inneb\xE6rer programmatisk tilgang til webressurser eller webtjenester ved \xE5\
  \ gj\xF8re foresp\xF8rsler over HTTP."
title: "Sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Slik gjør du:
Nøkkelen til å sende en HTTP-forespørsel i VBA er å utnytte `Microsoft XML, v6.0`-biblioteket (eller eldre versjoner, avhengig av systemet ditt). Sørg først for at denne referansen er aktivert i prosjektet ditt ved å gå til Verktøy > Referanser i VBA-editoren og merke av for `Microsoft XML, v6.0`.

Her er hvordan du sender en enkel HTTP GET-forespørsel:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Feil: " & .Status & " - " & .statusText
    End If
End With
```

For en POST-forespørsel, hvor vi trenger å sende data (f.eks., JSON) til en server:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""nøkkel"":""verdi""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Feil: " & .Status & " - " & .statusText
    End If
End With
```

Eksempelutdata for en vellykket forespørsel kan være en JSON-streng eller en HTML-side, avhengig av API-et eller nettsiden du samhandler med:

```
{"data": "Dette er responsen fra serveren"}
```

## Dypdykk
Metoden som vises utnytter `MSXML2.XMLHTTP`-objektet, en del av Microsoft XML Core Services (MSXML). Det ble introdusert for å tilby VBA-utviklere en måte å utføre XML-baserte operasjoner på og, over tid, ble det et vanlig verktøy for HTTP-forespørsler, selv når det ikke jobbes direkte med XML-data. Til tross for sin alder, forblir det et pålitelig alternativ for enkle webinteraksjoner i VBA.

Imidlertid mangler VBA og dens HTTP-forespørselsmekanismer robustheten og fleksibiliteten som finnes i moderne programmeringsmiljøer. For eksempel er håndtering av asynkrone forespørsler eller arbeid innenfor applikasjoner som krever avanserte HTTP-funksjoner (som websockets eller server-sent events) utenfor VBA sitt omfang. Når man jobber med mer komplekse webintegreringsprosjekter, utnytter utviklere ofte eksterne biblioteker eller verktøy, eller til og med automatiserer nettleseratferd via web scraping-teknikker, selv om disse er omveier snarere enn løsninger.

Språk og miljøer som Python med sitt `requests`-bibliotek eller JavaScript som kjører på Node.js tilbyr mer kraftfulle og allsidige HTTP-forespørselskapasiteter rett ut av boksen, inkludert asynkrone operasjoner, enklere JSON-håndtering og omfattende støtte for forskjellige webteknologier. Utviklere forankret i Microsoft-økosystemet kan vurdere å gå over til PowerShell eller C# for oppgaver som krever mer sofistikert webinteraksjon, og benytte .NETs omfattende nettverksprogrammeringsfunksjoner.

Dermed, mens VBA sine HTTP-forespørselskapasiteter er tilstrekkelige for enkle forespørsler og datainnhentingsoppgaver, blir det avgjørende å utforske alternativer når prosjektets krav utvikler seg mot det komplekse og moderne weblandskapet.
