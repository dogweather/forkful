---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:25.990569-07:00
description: "Het lezen van commandoregelargumenten in Visual Basic for Applications\
  \ (VBA) houdt in dat je toegang krijgt tot parameters die aan je programma worden\u2026"
lastmod: '2024-03-11T00:14:24.472588-06:00'
model: gpt-4-0125-preview
summary: "Het lezen van commandoregelargumenten in Visual Basic for Applications (VBA)\
  \ houdt in dat je toegang krijgt tot parameters die aan je programma worden\u2026"
title: Commandoregelargumenten lezen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het lezen van commandoregelargumenten in Visual Basic for Applications (VBA) houdt in dat je toegang krijgt tot parameters die aan je programma worden doorgegeven bij uitvoering. Deze techniek wordt vaak gebruikt om het gedrag of de uitvoer van een programma te beïnvloeden zonder de behoefte aan gebruikersinteractie, waardoor automatisering en scriptingtaken aanzienlijk eenvoudiger en veelzijdiger worden.

## Hoe:

In tegenstelling tot eenvoudigere programmeeromgevingen heeft VBA geen ingebouwde functie om commandoregelargumenten rechtstreeks op een conventionele manier te lezen, omdat het vooral is ontworpen voor integratie binnen Microsoft Office-applicaties. Echter, met een beetje creativiteit kunnen we Windows Script Host (WSH) of externe API's gebruiken om een vergelijkbare functionaliteit te bereiken. Hier is een praktische oplossing met behulp van WSH:

1. **Maak een VBScript om argumenten door te geven aan VBA:**

   Schrijf eerst een VBScript-bestand (*jeScript.vbs*) dat je VBA-applicatie start (bijv. een Excel-macro) en de commandoregelargumenten doorgeeft:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\JeMacroWerkboek.xlsm"
objExcel.Run "JeMacroNaam", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Toegang tot de argumenten in VBA:**

   In je VBA-applicatie (*JeMacroWerkboek.xlsm*), wijzig of creëer de macro (*JeMacroNaam*) om parameters te accepteren:

```vb
Sub JeMacroNaam(arg1 As String, arg2 As String)
    MsgBox "Argument 1: " & arg1 & " Argument 2: " & arg2
End Sub
```

3. **Voer je script uit:**

   Voer het VBScript uit vanaf de commandoregel, waarbij je argumenten doorgeeft zoals nodig:

```shell
cscript jeScript.vbs "Hallo" "Wereld"
```

   Dit zou moeten resulteren in het uitvoeren van je VBA-macro met de argumenten "Hallo" en "Wereld", en deze te tonen in een berichtvenster.

## Diepgaand:

In een historische context is VBA bedacht om de mogelijkheden van Microsoft Office-applicaties uit te breiden, niet als een zelfstandige programmeeromgeving. Als zodanig ligt directe interactie met de commandoregel buiten zijn primaire toepassingsgebied, wat het gebrek aan ingebouwde ondersteuning voor het lezen van commandoregelargumenten verklaart.

De hierboven geschetste methode, hoewel effectief, is meer een omweg dan een inheemse oplossing, waarbij externe scripting wordt gebruikt om de kloof te overbruggen. Deze aanpak kan complexiteit en potentiële beveiligingsproblemen met zich meebrengen omdat het inschakelen van macro's vereist is en mogelijk het verlagen van beveiligingsinstellingen om uit te voeren.

Voor taken die sterk afhankelijk zijn van commandoregelargumenten of die een naadlozere integratie met het Windows-besturingssysteem nodig hebben, kunnen andere programmeertalen zoals PowerShell of Python robuustere en veiligere oplossingen bieden. Deze alternatieven bieden directe ondersteuning voor commandoregelargumenten en zijn beter geschikt voor zelfstandige applicaties of scripts die externe input nodig hebben om hun gedrag dynamisch te wijzigen.
