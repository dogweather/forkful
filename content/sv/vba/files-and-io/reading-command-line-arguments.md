---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:18.111736-07:00
description: "Hur man g\xF6r: Till skillnad fr\xE5n mer raka programmeringsmilj\xF6\
  er, har VBA inte en inbyggd funktion f\xF6r att direkt l\xE4sa kommandoradsargument\
  \ i konventionell\u2026"
lastmod: '2024-03-13T22:44:37.759829-06:00'
model: gpt-4-0125-preview
summary: "Till skillnad fr\xE5n mer raka programmeringsmilj\xF6er, har VBA inte en\
  \ inbyggd funktion f\xF6r att direkt l\xE4sa kommandoradsargument i konventionell\
  \ bem\xE4rkelse eftersom det fr\xE4mst \xE4r designat f\xF6r inb\xE4ddning inom\
  \ Microsoft Office-applikationer."
title: "L\xE4sa kommandoradsargument"
weight: 23
---

## Hur man gör:
Till skillnad från mer raka programmeringsmiljöer, har VBA inte en inbyggd funktion för att direkt läsa kommandoradsargument i konventionell bemärkelse eftersom det främst är designat för inbäddning inom Microsoft Office-applikationer. Dock kan vi med lite kreativitet använda Windows Script Host (WSH) eller anropa externa API:er för att uppnå liknande funktionalitet. Här är en praktisk lösning med hjälp av WSH:

1. **Skapa ett VBScript för att skicka argument till VBA:**

   Först, skriv en VBScript-fil (*yourScript.vbs*) som startar din VBA-applikation (t.ex. en Excel-makro) och skickar kommandoradsargumenten:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Åtkomst till argumenten i VBA:**

   I din VBA-applikation (*YourMacroWorkbook.xlsm*), modifiera eller skapa makrot (*YourMacroName*) för att acceptera parametrar:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Argument 1: " & arg1 & " Argument 2: " & arg2
End Sub
```

3. **Kör ditt Script:**

   Exekvera VBScript från kommandoraden, skicka argument vid behov:

```shell
cscript yourScript.vbs "Hello" "World"
```

   Detta bör resultera i att ditt VBA-makro körs med argumenten "Hello" och "World", och visar dem i en meddelanderuta.

## Fördjupning:
I historiskt perspektiv utformades VBA för att utöka kapaciteterna hos Microsoft Office-applikationer, inte som en fristående programmeringsmiljö. Som sådan ligger direkt interaktion med kommandoraden utanför dess primära omfattning, vilket förklarar bristen på inbyggt stöd för att läsa kommandoradsargument.

Metoden som beskrivits ovan är mer av en lösning på omvägar än en infödd lösning, vilken utnyttjar externa script för att överbrygga gapet. Detta tillvägagångssätt kan införa komplexitet och potentiella säkerhetsproblem eftersom det kräver att makron är aktiverade och potentiellt sänker säkerhetsinställningarna för att köra.

För uppgifter som är starkt beroende av kommandoradsargument eller behöver mer sömlös integration med Windows-operativsystemet, kan andra programmeringsspråk som PowerShell eller Python erbjuda mer robusta och säkra lösningar. Dessa alternativ erbjuder direkt stöd för kommandoradsargument och är bättre lämpade för fristående applikationer eller script som kräver extern inmatning för att dynamiskt modifiera sitt beteende.
