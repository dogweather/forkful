---
title:                "Skrive tester"
aliases:
- no/vba/writing-tests.md
date:                  2024-02-01T22:08:50.167275-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skrive tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/vba/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å skrive tester i programmering innebærer å lage spesifikke prosedyrer for å validere funksjonaliteten og ytelsen til kode-segmentene dine, for å sikre at de fungerer som forventet under ulike forhold. Programmerere gjør dette for å fange opp feil tidlig, forbedre kodekvaliteten, og lette fremtidig kodevedlikehold og forbedringer.

## Hvordan:

Selv om Visual Basic for Applications (VBA) ikke leveres med et innebygd testrammeverk lik de som er tilgjengelige i språk som Python eller JavaScript, kan du fremdeles implementere enkle testprosedyrer for å sjekke integriteten til koden din. Her er et eksempel for å illustrere:

Anta at du har en funksjon i VBA som legger sammen to tall:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

For å teste denne funksjonen, kan du skrive en annen prosedyre som validerer utdataene mot forventede resultater:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Test Bestått!", vbInformation
    Else
        MsgBox "Test Feilet. Forventet 15 men fikk " & result, vbCritical
    End If
End Sub
```

Å kjøre `TestAddNumbers` vil vise en meldingsboks som indikerer om testen ble bestått eller ikke, basert på funksjonens utdata. Selv om dette er et forenklet scenario, kan du bygge mer komplekse tester ved å innlemme løkker, ulike inndataverdier, og teste for flere funksjoner.

## Dypdykk

Tilnærmingen til å skrive tester i VBA som er vist her, er manuell og mangler funksjonene til mer sofistikerte testrammeverk som er tilgjengelige i andre programmeringsmiljøer, som automatiserte testkjøringer, oppsett/nedbrytingsprosedyrer, og integrert rapportering av testresultater. Før den bredere adopsjonen av enhetstestrammeverk og testdrevet utvikling (TDD), var manuelle testprosedyrer lik den som er beskrevet vanlig. Selv om denne metoden er enkel og kan være effektiv for små prosjekter eller til læringsformål, er den ikke skalerbar eller effektiv for større prosjekter eller team.

I miljøer som støtter rikere utviklingsverktøy, henvender programmerere seg ofte til rammeverk som NUnit for .NET-applikasjoner eller JUnit for Java-applikasjoner, som gir omfattende verktøy for å systematisk skrive og kjøre tester. Disse rammeverkene tilbyr avanserte funksjoner som å hevde testresultater, sette opp mock-objekter, og måle kode dekning.

For VBA-utviklere som ser etter mer avanserte teste-kapasiteter, kan det nærmeste alternativet være å dra nytte av eksterne verktøy eller integrere med andre programmeringsmiljøer. Noen utviklere bruker VBA i tilknytning med Excel for å manuelt registrere testscenarier og resultater. Selv om det ikke er like praktisk eller automatisert som å bruke et dedikert testrammeverk, kan disse metodene delvis tette gapet og bidra til å opprettholde påliteligheten til VBA-løsninger i komplekse eller kritiske applikasjoner.
