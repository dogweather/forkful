---
date: 2024-01-26 04:08:54.836044-07:00
description: "Att anv\xE4nda en debugger inneb\xE4r att s\xE4tta brytpunkter, stega\
  \ igenom din kod, \xF6vervaka variabler och inspektera tillst\xE5ndet i ditt program\
  \ n\xE4r det k\xF6rs.\u2026"
lastmod: '2024-03-13T22:44:38.130411-06:00'
model: gpt-4-0125-preview
summary: "Att anv\xE4nda en debugger inneb\xE4r att s\xE4tta brytpunkter, stega igenom\
  \ din kod, \xF6vervaka variabler och inspektera tillst\xE5ndet i ditt program n\xE4\
  r det k\xF6rs.\u2026"
title: "Att anv\xE4nda en debugger"
---

{{< edit_this_page >}}

## Vad & Varför?
Att använda en debugger innebär att sätta brytpunkter, stega igenom din kod, övervaka variabler och inspektera tillståndet i ditt program när det körs. Det är en omvälvande sak för programmerare eftersom det pekar ut buggar och hjälper oss att förstå vad vår kod verkligen håller på med.

## Hur till:
I PowerShell kan du felsöka skript med den inbyggda PowerShell Integrated Scripting Environment (ISE) eller Visual Studio Code (VS Code) med PowerShell-tillägget. Så här använder du brytpunkter i båda:

### PowerShell ISE:
```PowerShell
# Ställ in en brytpunkt på en specifik rad
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Kör ditt skript som vanligt
.\MyScript.ps1

# När skriptet träffar brytpunkten kan du inspektera variabler
$myVariable

# Fortsätt utförandet
Continue
```

### Visual Studio Code:
```PowerShell
# Öppna ditt PowerShell-skript i VS Code.
# Klicka till vänster om radnumret för att sätta en brytpunkt.
# Starta debugging genom att trycka på F5 eller klicka på 'Start Debugging'.

# VS Code kommer att stoppa utförandet vid din brytpunkt.
# Använd debugpanelen för att övervaka variabler, inspektera anropsstacken och kontrollera flödet.
```

Felsökning i båda miljöerna låter dig stega in (F11), stega över (F10) och stega ut (Shift+F11) under felsökningen.

## Djupdykning
Historiskt sett var felsökning i PowerShell lite klumpig; det krävdes många `Write-Host`-rader för att utskrifta variabeltillstånd eller den klassiska metoden med försök och misstag. Med introduktionen av PowerShell ISE, och mer nyligen, VS Code med dess rika felsökningsfunktioner, blev PowerShell-felsökning nästan lika intuitiv som i fullfjädrade programmeringsspråk.

Alternativ till PowerShell:s inbyggda felsökningsverktyg inkluderar tredjepartsverktyg som PowerGUI eller användning av robusta IDE:s som Visual Studio med ett PowerShell-tillägg.

När du implementerar en debugger, överväg skriptets räckvidd, särskilt när du arbetar med punktskript eller moduler. Brytpunkter kan vara villkorade, baserade på variabeländringar eller radbaserade, vilket möjliggör exakt kontroll under en felsökningssession.

Dessutom, med övergången till PowerShell Core (plattformsoberoende PowerShell), har felsökningen till stor del flyttat till händerna på VS Code, som erbjuder en konsekvent upplevelse över olika plattformar.

## Se även
För mer om felsökning i PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
