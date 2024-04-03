---
date: 2024-01-26 04:25:04.496779-07:00
description: "Hur man g\xF6r: I PowerShell finns det inget inbyggt cmdlet f\xF6r att\
  \ tolka TOML. Du skulle typiskt anv\xE4nda en modul eller konvertera TOML till JSON\
  \ med ett\u2026"
lastmod: '2024-03-13T22:44:38.149397-06:00'
model: gpt-4-0125-preview
summary: "I PowerShell finns det inget inbyggt cmdlet f\xF6r att tolka TOML."
title: Att arbeta med TOML
weight: 39
---

## Hur man gör:
I PowerShell finns det inget inbyggt cmdlet för att tolka TOML. Du skulle typiskt använda en modul eller konvertera TOML till JSON med ett verktyg som `toml-to-json` om du vill arbeta med PowerShell. Så här gör du det med en påhittad modul `PowerShellTOML`:

```PowerShell
# Först, installera modulen (påhittad, för demonstration)
Install-Module PowerShellTOML

# Importera en TOML-fil
$config = Import-TomlConfig -Path './config.toml'

# Att komma åt ett värde
Write-Output $config.database.server

# Exempel på TOML-innehåll i 'config.toml':
# [databas]
# server = "192.168.1.1"
# portar = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Exempel på utdata:
# 192.168.1.1
```

## Fördjupning
TOML skapades av Tom Preston-Werner, medgrundare av GitHub, som ett enklare alternativ till XML och YAML för konfigurationsfiler. Dess första version dök upp 2013. TOML är jämförbart med JSON men är utformat för att vara mer människovänligt, vilket gör det till ett bra val för konfiguration som underhålls av människor. Alternativ inkluderar YAML, JSON och XML.

I termer av implementering skulle en PowerShell-modul för TOML typiskt vara en wrapper runt ett TOML-bibliotek skrivet i ett mer prestandaorienterat språk som C#. PowerShell har inte inbyggt stöd för TOML, vilket är varför en sådan modul är nödvändig för att enkelt kunna interagera med TOML-formatet.

## Se även
- TOML-standard: https://toml.io/en/
- GitHub-repositorium för `toml` PowerShell-modul (om det finns vid lästidpunkten): https://github.com/powershell/PowerShellTOML
- En introduktion till TOML: https://github.com/toml-lang/toml
- Jämförelse av data-serialiseringsformat: https://sv.wikipedia.org/wiki/J%C3%A4mf%C3%B6relse_av_data-serialiseringsformat
