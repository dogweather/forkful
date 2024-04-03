---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:12.801023-07:00
description: "Hvordan: I VBA kan du analysere HTML ved \xE5 bruke `Microsoft HTML\
  \ Object Library`. Legg til en referanse til dette biblioteket i VBA-editoren din\
  \ ved \xE5 g\xE5\u2026"
lastmod: '2024-03-13T22:44:40.617062-06:00'
model: gpt-4-0125-preview
summary: "I VBA kan du analysere HTML ved \xE5 bruke `Microsoft HTML Object Library`."
title: Analysering av HTML
weight: 43
---

## Hvordan:
I VBA kan du analysere HTML ved å bruke `Microsoft HTML Object Library`. Legg til en referanse til dette biblioteket i VBA-editoren din ved å gå til Verktøy > Referanser og krysse av for `Microsoft HTML Object Library`. Dette gir deg tilgang til klasser for å navigere og manipulere HTML-dokumenter.

Her er et enkelt eksempel som viser hvordan du laster et HTML-dokument fra en fil og trekker ut alle lenkene (ankertagger):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' Last inn HTML-innhold fra en fil
    htmlFile = "C:\sti\til\din\fil.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' Initialiser HTML-dokument
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Få alle anker-tagger
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Loop gjennom alle anker-elementer og skriv ut href-attributten
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Dette skriptet leser innholdet av en HTML-fil, laster den inn i et `HTMLDocument`-objekt, henter alle ankrelementer (`<a>`-tagger), og itererer deretter over dem, og skriver ut `href`-attributtet til hver i umiddelbart vindu.

## Dypdykk:
Historisk sett har det å analysere HTML i VBA vært litt tungvint på grunn av mangel på direkte støtte for moderne web-skraping og dokumenthåndteringsteknologier. Microsoft HTML Object Library, til tross for å være kraftfull, er noe datert og kan ikke håndtere moderne webstandarder så smidig som nyere teknologier.

For komplekse HTML-analyse- og webskrapingsoppgaver, anbefales ofte alternative verktøy og språk som Python med biblioteker som Beautiful Soup eller Scrapy. Disse moderne verktøyene tilbyr mer fleksibilitet, bedre ytelse og er mer i tråd med gjeldende webstandarder. Imidlertid, når du jobber innenfor Microsoft Office-økosystemet, forblir det å bruke VBA med Microsoft HTML Object Library en verdifull ferdighet. Det låser opp direkte manipulasjon av HTML-innhold på en måte som integreres sømløst med applikasjoner som Excel og Access, og gir en enkel metode for å utføre oppgaver som involverer grunnleggende håndtering av HTML-dokumenter uten å måtte gå ut av det kjente VBA-miljøet.
