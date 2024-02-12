---
title:                "Kontrollera om en katalog existerar"
aliases:
- /sv/powershell/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:13.191332-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad och varför?
I PowerShell är kontrollen om en katalog finns en vanlig uppgift som hjälper skript att fatta beslut baserat på filsystemets struktur—till exempel för att undvika fel genom att bekräfta att en målkatalog finns på plats innan man försöker läsa från eller skriva till den. Det är avgörande för att säkerställa att ditt skript beter sig pålitligt i olika miljöer.

## Hur man gör:
PowerShell erbjuder ett enkelt sätt att kontrollera närvaron av en katalog med hjälp av cmdleten `Test-Path`. Denna cmdlet returnerar ett booleskt värde som indikerar om den angivna sökvägen finns. Så här kan du använda den:

```powershell
# Kontrollera om en katalog finns
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "Finns katalogen? $directoryExists"
```

Exempelutskrift för en katalog som finns:

```
Finns katalogen? True
```

Och för en katalog som inte finns:

```
Finns katalogen? False
```

För mer komplexa skript, särskilt de som interagerar med nätverksdelningar eller molnlagring, kan du behöva ytterligare kontroller eller funktionalitet som inte direkt är tillgänglig genom `Test-Path`. I sådana fall kan det vara fördelaktigt att använda tredjepartsmoduleller eller bibliotek för PowerShell, även om de flesta rutinuppgifter kan utföras med PowerShell:s inbyggda cmdlets. Fram till min sista kunskapsuppdatering har det inte funnits något allmänt accepterat tredjepartsbibliotek specifikt för att kontrollera katalogexistens bortom vad `Test-Path` erbjuder, främst eftersom `Test-Path` i sig är både robust och effektivt för detta ändamål.
