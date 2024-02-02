---
title:                "Lese kommandolinje-argumenter"
date:                  2024-02-01T21:59:30.494620-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lese kommandolinje-argumenter"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/vba/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese kommandolinjeargumenter i Visual Basic for Applications (VBA) innebærer å få tilgang til parametere som er sendt til programmet ditt ved utførelse. Denne teknikken brukes ofte for å påvirke oppførselen eller utdataene til et program uten behov for brukerinteraksjon, noe som gjør automatisering og skripting betydelig enklere og mer allsidig.

## Hvordan:

I motsetning til mer rettframme programmeringsmiljøer, har ikke VBA en innebygd funksjon for å direkte lese kommandolinjeargumenter i tradisjonell forstand fordi det primært er designet for å bli integrert innen Microsoft Office-applikasjoner. Imidlertid, med litt kreativitet, kan vi bruke Windows Script Host (WSH) eller kalle eksterne APIer for å oppnå lignende funksjonalitet. Her er en praktisk omgåelse ved bruk av WSH:

1. **Lag et VBScript for å Sende Argumenter til VBA:**

   Først, skriv en VBScript-fil (*dittSkript.vbs*) som starter VBA-applikasjonen din (f.eks. en Excel-makro) og sender kommandolinjeargumentene:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Få Tilgang til Argumentene i VBA:**

   I VBA-applikasjonen din (*DinMakroArbeidsbok.xlsm*), endre eller opprett makroen (*DinMakroNavn*) for å akseptere parametere:

```vb
Sub DinMakroNavn(arg1 As String, arg2 As String)
    MsgBox "Argument 1: " & arg1 & " Argument 2: " & arg2
End Sub
```

3. **Kjør Ditt Skript:**

   Utfør VBScriptet fra kommandolinjen, ved å sende argumenter som nødvendig:

```shell
cscript dittSkript.vbs "Hallo" "Verden"
```

   Dette bør resultere i at VBA-makroen din blir utført med argumentene "Hallo" og "Verden", som vises i en meldingsboks.

## Dypdykk:

I en historisk kontekst ble VBA utviklet for å utvide funksjonaliteten til Microsoft Office-applikasjoner, ikke som et selvstendig programmeringsmiljø. Som sådan, er direkte interaksjon med kommandolinjen utenfor dets primære omfang, noe som forklarer mangelen på innebygd støtte for å lese kommandolinjeargumenter.

Metoden som er skissert ovenfor, selv om den er effektiv, er mer en omvei enn en naturlig løsning, og tar i bruk eksternt skripting for å tette gapet. Denne tilnærmingen kan introdusere kompleksitet og potensielle sikkerhetsproblemer, da det krever at makroer aktiveres og potensielt redusere sikkerhetsinnstillinger for å utføre.

For oppgaver som er sterkt avhengige av kommandolinjeargumenter, eller som trenger en mer sømløs integrasjon med Windows-operativsystemet, kan andre programmeringsspråk som PowerShell eller Python tilby mer robuste og sikre løsninger. Disse alternativene gir direkte støtte for kommandolinjeargumenter og er bedre egnet for selvstendige applikasjoner eller skript som krever ekstern inndata for å dynamisk endre oppførselen.
