---
date: 2024-01-26 03:50:58.380537-07:00
description: "\xC5 bruke en feils\xF8ker betyr \xE5 sette brytepunkter, g\xE5 gjennom\
  \ koden din trinn for trinn, overv\xE5ke variabler og inspisere tilstanden til programmet\
  \ ditt mens\u2026"
lastmod: '2024-03-13T22:44:41.021950-06:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en feils\xF8ker betyr \xE5 sette brytepunkter, g\xE5 gjennom\
  \ koden din trinn for trinn, overv\xE5ke variabler og inspisere tilstanden til programmet\
  \ ditt mens\u2026"
title: "\xC5 bruke en feils\xF8ker"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å bruke en feilsøker betyr å sette brytepunkter, gå gjennom koden din trinn for trinn, overvåke variabler og inspisere tilstanden til programmet ditt mens det kjører. Det er et vendepunkt for programmerere fordi det peker på feil og hjelper oss å forstå hva koden vår faktisk gjør.

## Hvordan:
I PowerShell kan du feilsøke skript ved å bruke den innebygde PowerShell Integrated Scripting Environment (ISE) eller Visual Studio Code (VS Code) med PowerShell-utvidelsen. Slik bruker du brytepunkter i begge:

### PowerShell ISE:
```PowerShell
# Sett et brytepunkt på en spesifikk linje
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Kjør skriptet ditt som normalt
.\MyScript.ps1

# Når skriptet treffer brytepunktet, kan du inspisere variabler
$myVariable

# Fortsett utførelsen
Continue
```

### Visual Studio Code:
```PowerShell
# Åpne PowerShell-skriptet ditt i VS Code.
# Klikk til venstre for linjenummeret for å sette et brytepunkt.
# Start feilsøkingen ved å trykke F5 eller klikke på 'Start Debugging'.

# VS Code vil stoppe utførelsen ved brytepunktet ditt.
# Bruk feilsøkepanelet for å overvåke variabler, inspisere anropsstabelen og kontrollere flyten.
```

Feilsøking i begge miljøer lar deg steg inn (F11), steg over (F10), og steg ut (Shift+F11) mens du feilsøker.

## Dypdykk
Historisk sett var feilsøking i PowerShell litt klønete; det krevde mange `Write-Host`-linjer for å utskrive tilstander til variabler eller den klassiske prøv-og-feil-metoden. Med adventen av PowerShell ISE, og mer nylig, VS Code med sine rike feilsøkingsfunksjoner, ble feilsøking i PowerShell nesten like intuitivt som i fullverdige programmeringsspråk.

Alternativer til PowerShell sine innebygde feilsøkingsverktøy inkluderer tredjepartsverktøy som PowerGUI eller bruk av robuste IDEer som Visual Studio med en PowerShell-plugin.

Når du implementerer en feilsøker, vurder skriptomfanget, spesielt når du arbeider med prikk-innlastede skript eller moduler. Brytepunkter kan være basert på betingelser, basert på variabelendringer, eller basert på linje, noe som tillater presis kontroll under en feilsøkingssesjon.

Videre, med overgangen til PowerShell Core (plattformuavhengig PowerShell), har feilsøking i stor grad flyttet inn i hendene på VS Code, som gir en konsekvent opplevelse på forskjellige plattformer.

## Se også
For mer om feilsøking i PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
