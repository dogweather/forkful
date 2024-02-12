---
title:                "Att arbeta med TOML"
aliases:
- /sv/powershell/working-with-toml.md
date:                  2024-01-26T04:25:04.496779-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?

TOML, en förkortning för Toms Uppenbara, Minimala Språk, är ett data-serialiseringsformat som är lätt att läsa tack vare dess klara semantik. Programutvecklare använder det för konfigurationsfiler eftersom det erbjuder en balans mellan att vara läsligt för människor och maskinvänligt.

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
