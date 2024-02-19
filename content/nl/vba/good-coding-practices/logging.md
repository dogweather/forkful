---
aliases:
- /nl/vba/logging/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:00.378516-07:00
description: "Loggen in Visual Basic for Applications (VBA) houdt in dat er informatie\
  \ over het runtimegedrag van een programma wordt vastgelegd in een bestand, console\u2026"
lastmod: 2024-02-18 23:09:01.673131
model: gpt-4-0125-preview
summary: "Loggen in Visual Basic for Applications (VBA) houdt in dat er informatie\
  \ over het runtimegedrag van een programma wordt vastgelegd in een bestand, console\u2026"
title: Logboekregistratie
---

{{< edit_this_page >}}

## Wat & Waarom?

Loggen in Visual Basic for Applications (VBA) houdt in dat er informatie over het runtimegedrag van een programma wordt vastgelegd in een bestand, console of database. Programmeurs gebruiken loggen om hun applicaties te monitoren, problemen te diagnosticeren en de prestatiekenmerken te begrijpen.

## Hoe te:

In VBA is er niet een ingebouwd logkader zoals gevonden in sommige andere talen. Echter, het implementeren van een eenvoudig logmechanisme is eenvoudig. Hieronder volgt een voorbeeld van hoe een basisbestandslogger te creëren.

1. **Schrijven naar een Logbestand**: Dit voorbeeldfunctie, `LogMessage`, schrijft berichten naar een tekstbestand met een tijdstempel.

```basic
Sub LogMessage(message As String)
    Dim logFilePath As String
    Dim fileNum As Integer
    
    ' Geef het pad van het logbestand op
    logFilePath = ThisWorkbook.Path & "\log.txt"
    
    ' Haal het volgende beschikbare bestandsnummer op
    fileNum = FreeFile()
    
    ' Open het bestand om toe te voegen
    Open logFilePath For Append As #fileNum
    
    ' Schrijf de tijdstempel en het logbericht
    Print #fileNum, Now & ": " & message
    
    ' Sluit het bestand
    Close #fileNum
End Sub
```

Om een bericht te loggen, bel gewoon `LogMessage("Uw bericht hier")`. Dit produceert vermeldingen in *log.txt* zoals:

```
30-4-2023 15:45:32: Uw bericht hier
```

2. **Lezen uit een Logbestand**: Om de inhoud van het logbestand te lezen en weer te geven:

```basic
Sub ReadLogFile()
    Dim logFilePath As String
    Dim fileContent As String
    Dim fileNum As Integer
    
    logFilePath = ThisWorkbook.Path & "\log.txt"
    fileNum = FreeFile()
    
    ' Open het bestand om te lezen
    Open logFilePath For Input As #fileNum
    
    ' Lees de volledige bestandsinhoud
    fileContent = Input(LOF(fileNum), fileNum)
    
    ' Sluit het bestand
    Close #fileNum
    
    ' Toon de bestandsinhoud
    MsgBox fileContent
End Sub
```

## Diepgaand

Loggen in VBA, vanwege het gebrek aan een native logframework, wordt meestal geïmplementeerd via basisbestandsoperaties of door gebruik te maken van de kracht van externe COM-objecten voor meer geavanceerde behoeften, zoals loggen in een database of interactie met het Windows Event Log. Historisch gezien is loggen in VBA een manier geweest om de beperkingen te omzeilen die worden opgelegd door zijn simplistische foutafhandeling en debugtools. Hoewel effectief, is het direct manipuleren van bestanden voor loggen rudimentair en kan het inefficiënt zijn bij grote hoeveelheden gegevens of onder hoge concurrency. Voor meer geavanceerde logmogelijkheden wenden programmeurs zich vaak tot externe bibliotheken of integreren ze met systemen die specifiek zijn ontworpen voor loggen, zoals de ELK-stack (Elasticsearch, Logstash, Kibana) of Splunk, via webservice-oproepen of tussenliggende databases. Hoewel VBA niet de moderne gemakken biedt die in nieuwere programmeertalen worden gevonden, stelt het begrijpen van zijn mogelijkheden en beperkingen programmeurs in staat om loggen effectief te gebruiken als een krachtige tool voor applicatiemonitoring en diagnostiek.
