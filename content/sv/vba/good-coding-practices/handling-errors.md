---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:13.374719-07:00
description: "Felhantering i Visual Basic for Applications (VBA) syftar till processen\
  \ att f\xF6rutse, uppt\xE4cka och l\xF6sa programmerings-, applikations- eller\u2026"
lastmod: '2024-03-13T22:44:37.751109-06:00'
model: gpt-4-0125-preview
summary: "Felhantering i Visual Basic for Applications (VBA) syftar till processen\
  \ att f\xF6rutse, uppt\xE4cka och l\xF6sa programmerings-, applikations- eller kommunikationsfel."
title: Hantera fel
weight: 16
---

## Vad & Varför?

Felhantering i Visual Basic for Applications (VBA) syftar till processen att förutse, upptäcka och lösa programmerings-, applikations- eller kommunikationsfel. Implementering av robust felhantering är avgörande för att bibehålla applikationers integritet och förbättra användarupplevelsen genom att elegant hantera oväntade problem utan att orsaka plötsliga krascher eller datförlust.

## Hur:

I VBA implementeras felhantering vanligtvis med hjälp av uttalandet `On Error` som instruerar VBA hur man ska fortsätta när ett fel uppstår. De vanligaste strategierna för felhantering involverar `On Error GoTo` etikett, `On Error Resume Next` och `On Error GoTo 0`.

**Exempel 1: Användning av `On Error GoTo`**

Detta tillvägagångssätt låter dig dirigera programmet till en specifik kodsektion, märkt direkt efter att ett fel har uppstått.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' Detta kommer att orsaka ett nolldivisionsfel

    Exit Sub
ErrHandler:
    MsgBox "Ett fel inträffade: " & Err.Description, vbCritical, "Fel!"
    Resume Next
End Sub
```

I det här exemplet kommer alla körtidsfel att utlösa hoppet till `ErrHandler`, visa ett felmeddelande och sedan fortsätta med nästa rad efter felet.

**Exempel 2: Användning av `On Error Resume Next`**

Denna strategi instruerar VBA att fortsätta utföra nästa rad kod även om ett fel uppstår, vilket kan vara användbart för fel som förväntas vara ofarliga eller när du planerar att hantera felet senare i utförandet.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' Detta kommer inte att få programmet att stoppa; felet ignoreras
    
    ' Kontrollera om ett fel inträffade
    If Err.Number <> 0 Then
        MsgBox "Ett fel inträffade: " & Err.Description, vbExclamation, "Hanterat fel"
        ' Återställ fel
        Err.Clear
    End If
End Sub
```

I det här fallet bryts inte programmet vid fel; det kontrollerar om ett fel har inträffat, hanterar det om det har gjort det, och nollställer sedan felet.

## Fördjupning

Historiskt sett har felhantering i programmeringsspråk utvecklats från enkla goto-uttalanden till mer sofistikerade mekanismer som undantag (exceptions) i språk såsom Java och C#. VBA:s felhantering, även om den inte är lika kraftfull eller flexibel som modern undantagshantering, uppfyller sitt syfte inom kontexten av språkets användningsområden för att automatisera uppgifter i Microsoft Office-miljöer.

Den främsta begränsningen med VBA:s felhantering ligger i dess något omständliga och manuella tillvägagångssätt, vilket kräver noggrann placering av felhanteringskod och klar förståelse för utförandets flöde. Moderna programmeringsspråk erbjuder vanligtvis mer eleganta lösningar, såsom try-catch-block, som automatiskt hanterar flödet till felhanteringskoden utan behov av manuella kontroller eller hopp i körningen.

Trots dessa begränsningar är VBA:s felhanteringsmekanismer lämpliga för de flesta automatiseringsuppgifter och när de används korrekt kan de avsevärt minska sannolikheten för att ohanterade fel orsakar problem för användarna. Dessutom kan förståelse för VBA:s felhantering ge insikter i äldre programmeringsparadigm och utvecklingen av strategier för felhantering i programvaruutveckling.
