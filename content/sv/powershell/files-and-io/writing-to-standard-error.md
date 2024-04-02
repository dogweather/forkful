---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:16.677984-07:00
description: "Att skriva till standardfel (stderr) i PowerShell inneb\xE4r att skicka\
  \ felmeddelanden eller diagnostik direkt till stderr-str\xF6mmen, separat fr\xE5\
  n\u2026"
lastmod: '2024-03-13T22:44:38.142021-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i PowerShell inneb\xE4r att skicka\
  \ felmeddelanden eller diagnostik direkt till stderr-str\xF6mmen, separat fr\xE5\
  n\u2026"
title: Skriva till standardfel
weight: 25
---

## Vad & Varför?

Att skriva till standardfel (stderr) i PowerShell innebär att skicka felmeddelanden eller diagnostik direkt till stderr-strömmen, separat från standardutdataströmmen (stdout). Denna separation möjliggör en mer exakt kontroll över ett skripts utdata, vilket gör det möjligt för utvecklare att rikta normala och felmeddelanden till olika destinationer, vilket är grundläggande för felhantering och loggning.

## Hur man gör:

PowerShell förenklar processen att skriva till stderr genom användning av cmdleten `Write-Error` eller genom att rikta utdatan till metoden `$host.ui.WriteErrorLine()`. Dock, för direkt stderr-omdirigering, kan du föredra att använda .NET-metoder eller fildeskriptorsomdirigering som erbjuds av själva PowerShell.

**Exempel 1:** Använda `Write-Error` för att skriva ett felmeddelande till stderr.

```powershell
Write-Error "Detta är ett felmeddelande."
```

Utdata till stderr:
```
Write-Error: Detta är ett felmeddelande.
```

**Exempel 2:** Använda `$host.ui.WriteErrorLine()` för direkt skrivning till stderr.

```powershell
$host.ui.WriteErrorLine("Direkt skrivning till stderr.")
```

Utdata till stderr:
```
Direkt skrivning till stderr.
```

**Exempel 3:** Använda .NET-metoder för skrivning till stderr.

```powershell
[Console]::Error.WriteLine("Använder .NET-metod för stderr")
```

Denna metods utdata:
```
Använder .NET-metod för stderr
```

**Exempel 4:** Omdirigera felutdata med fildeskriptor `2>`.

Fildeskriptorer i PowerShell kan omdirigera olika strömmar. För stderr är filbeskrivaren `2`. Här är ett exempel på omdirigering av stderr till en fil med namnet `error.log` samtidigt som ett kommando körs som genererar ett fel.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

Detta exempel producerar ingen konsolutdata, men genererar en fil `error.log` i den aktuella katalogen som innehåller felmeddelandet från försöket att komma åt en fil som inte finns.

Sammanfattningsvis erbjuder PowerShell flera metoder för effektiv skrivning och hantering av felutdata, vilket möjliggör sofistikerade strategier för felhantering och loggning i skript och applikationer.
